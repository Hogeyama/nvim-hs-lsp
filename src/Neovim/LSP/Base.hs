
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Neovim.LSP.Base where

import           RIO
import qualified RIO.HashMap              as HM
import           RIO.List.Partial         (init)
import qualified RIO.Map                  as M

import           Control.Lens             (makeLenses, views)
import           Control.Lens.Operators
import           Control.Monad            (forM, forM_, forever)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Data.Aeson               as J
import           Data.Constraint          (withDict)
import           Data.Extensible.Rexport
import           Data.Singletons          (Sing, SomeSing (..), fromSing,
                                           singByProxy, toSing)
import           GHC.Stack
import           GHC.TypeLits             (Symbol)
import           System.IO                (openFile)
import           System.Process           (ProcessHandle, terminateProcess)

import           Neovim                   hiding (Plugin, (<>))
import qualified Neovim.Context.Internal  as Internal
import           Neovim.LSP.Protocol.Type

-------------------------------------------------------------------------------
-- Env
-------------------------------------------------------------------------------

type NeovimLsp = Neovim LspEnv

-- | Enviroment of the main thread.
type LspEnv = OrigRecord
  '[ "serverHandles"    >: TVar (Maybe ServerHandles)
   , "fileType"         >: TVar (Maybe String)
   , "otherHandles"     >: TVar OtherHandles
   , "inChan"           >: TChan ByteString
   , "outChan"          >: TChan ByteString
   , "context"          >: TVar Context
   , "logFunc"          >: LogFunc
   , "logFileHandle"    >: Handle
   , "logFuncFinalizer" >: IO ()
   ]

initialEnvM :: MonadIO m => m LspEnv
initialEnvM = do
  h <- liftIO $ openFile "/tmp/nvim-hs-lsp.log" AppendMode
  (lf, lfFinalizer) <- liftIO $ makeLogFunc h
  hsequence $ #serverHandles    <@=> newTVarIO Nothing
           <! #fileType         <@=> newTVarIO Nothing
           <! #otherHandles     <@=> newTVarIO (OtherHandles [])
           <! #inChan           <@=> newTChanIO
           <! #outChan          <@=> newTChanIO
           <! #context          <@=> newTVarIO initialContext
           <! #logFunc          <@=> return lf
           <! #logFileHandle    <@=> return h
           <! #logFuncFinalizer <@=> return lfFinalizer
           <! nil
  where
    makeLogFunc h = do
        hSetBuffering h LineBuffering
        opts <- setLogTerminal False .
                setLogUseColor False <$>
                logOptionsHandle h True
        newLogFunc opts

-- | Enviroment of each plugin.
type PluginEnv = OrigRecord
  '[ "inChan"  >: TChan InMessage
   , "outChan" >: TChan ByteString
   , "context" >: TVar Context
   , "logFunc" >: LogFunc
   ]

-- | Context shared with main thread and all plugins.
type Context = OrigRecord
  '[ "idMethodMap"    >: Map ID ClientRequestMethod
   , "uniqueID"       >: Int
   , "uniqueVersion"  >: Version
   , "versionMap"     >: Map Uri Version
   , "callbacks"      >: Map ID Callback
   , "lspConfig"      >: LspConfig
   , "diagnosticsMap" >: Map Uri [Diagnostic]
   , "openedFiles"    >: Map Uri Version
   ]

data Callback where
  Callback :: Typeable m => TMVar a -> CallbackOf m a -> Callback
type CallbackOf m a = ServerResponse m -> PluginAction a

initialContext :: Context
initialContext =
     #idMethodMap    @= M.empty
  <! #uniqueID       @= 0
  <! #uniqueVersion  @= 0
  <! #versionMap     @= M.empty
  <! #callbacks      @= M.empty
  <! #lspConfig      @= defaultLspConfig
  <! #diagnosticsMap @= M.empty
  <! #openedFiles    @= M.empty
  <! nil

data ServerHandles = ServerHandles
  { serverIn  :: Handle
  , serverOut :: Handle
  , serverErr :: Handle
  , serverProcHandle  :: ProcessHandle
  }

-- sender, receiver, watcher of serverErr, dispatcher, plugins
newtype OtherHandles = OtherHandles
  { unOtherHandles :: [(String, Async ())] }
  -- 'String' refers to the name of Handle

data OtherState = OtherState
  { _diagnosticsMap :: Map Uri [Diagnostic]
  }

data LspConfig = LspConfig
  { _autoLoadQuickfix :: Bool
  , _settingsPath :: Maybe FilePath
  , _lspCommands :: Map Language [String]
  }
defaultLspConfig :: LspConfig
defaultLspConfig =  LspConfig
  { _autoLoadQuickfix = False
  , _settingsPath = Nothing
  , _lspCommands = M.empty
  }

type Language = Text

-------------------------------------------------------------------------------
-- Plugin
-------------------------------------------------------------------------------

type PluginAction = Neovim PluginEnv

data Plugin = Plugin
  { pluginName   :: String
  , pluginAction :: PluginAction ()
  }

data InMessage where
  SomeNoti :: ImplNotification m => ServerNotification m -> InMessage
  SomeReq  :: ImplRequest      m => ServerRequest      m -> InMessage
  SomeResp :: ImplResponse     m => ServerResponse     m -> InMessage
deriving instance Show InMessage

instance J.ToJSON InMessage where
  toJSON (SomeNoti x) = J.toJSON x
  toJSON (SomeReq  x) = J.toJSON x
  toJSON (SomeResp x) = J.toJSON x

data InMessageMethod
  = InReq  ServerRequestMethod
  | InNoti ServerNotificationMethod
  | InResp ClientRequestMethod
  deriving (Show)

methodOf :: InMessage -> InMessageMethod
methodOf (SomeNoti noti) = InNoti $ fromSing $ singByProxy noti
methodOf (SomeReq  req ) = InReq  $ fromSing $ singByProxy req
methodOf (SomeResp resp) = InResp $ fromSing $ singByProxy resp

parseMessage :: Map ID ClientRequestMethod -> J.Value -> Either String InMessage
parseMessage map' v@(J.Object o) = mmethod >>= \case
    InReq (toSing -> SomeSing (s :: Sing m))
      -> withDict (prfServerReq s) $ SomeReq  @m <$> fromJSONEither v
    InNoti (toSing -> SomeSing (s :: Sing m))
      -> withDict (prfServerNoti s) $ SomeNoti @m <$> fromJSONEither v
    InResp (toSing -> SomeSing (s :: Sing m))
      -> withDict (prfServerResp s) $ SomeResp @m <$> fromJSONEither v
  where
    mmethod :: Either String InMessageMethod
    mmethod
      | HM.member "id" o
      , Just m <- fromJSONMay =<< HM.lookup "method" o
          = return $ InReq m
      | Just m <- fromJSONMay =<< HM.lookup "method" o
          = return $ InNoti m
      | Just m <- M.lookup `flip` map' =<< fromJSONMay =<< HM.lookup "id" o
          = return $ InResp m
      | otherwise
          = Left "そんなバナナ1"
parseMessage _ _ = Left "そんなバナナ2"

fromJSONEither :: J.FromJSON a => J.Value -> Either String a
fromJSONEither = resultEither . J.fromJSON

fromJSONMay :: J.FromJSON a => J.Value -> Maybe a
fromJSONMay = resultMaybe . J.fromJSON

resultMaybe :: J.Result a -> Maybe a
resultMaybe (J.Success x) = Just x
resultMaybe _             = Nothing

resultEither :: J.Result a -> Either String a
resultEither (J.Success x) = Right x
resultEither (J.Error e)   = Left e

-------------------------------------------------------------------------------
-- Lens
-------------------------------------------------------------------------------

makeLenses ''OtherState
makeLenses ''LspConfig

type family RecFields' env :: [Assoc Symbol *] where RecFields' (OrigRecord xs) = xs
type IsRecord' env = OrigRecord (RecFields' env) ~ env
type Associate' k v env = (IsRecord' env, Associate k v (RecFields' env))
type HasContext env = Associate' "context" (TVar Context)     env
type HasInChan  env = Associate' "inChan"  (TChan InMessage)  env
type HasOutChan env = Associate' "outChan" (TChan ByteString) env
instance Associate "logFunc" LogFunc xs
  => HasLogFunc (OrigRecord xs) where logFuncL = #logFunc

useTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> m a
useTV l = readTVarIO =<< view l

usesTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> (a -> b) -> m b
usesTV l f = f <$> useTV l

assignTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> a -> m ()
assignTV l x = do
  v <- view l
  atomically $ writeTVar v x

modifyOverTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> (a -> a) -> m ()
modifyOverTV l f = do
  v <- view l
  atomically $ modifyTVar' v f

(.==) :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> a -> m ()
(.==) = assignTV
infix 4 .==

(%==) :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> (a -> a) -> m ()
(%==) = modifyOverTV
infix 4 %==

-------------------------------------------------------------------------------
-- Finalize
-------------------------------------------------------------------------------

finalize :: NeovimLsp ()
finalize = do
    mapM_ (cancel.snd) =<< usesTV #otherHandles unOtherHandles
    useTV #serverHandles >>= \case
      Nothing -> return ()
      Just sh -> liftIO $ terminateProcess (serverProcHandle sh)
    liftIO =<< view #logFuncFinalizer
    hClose =<< view #logFileHandle

-------------------------------------------------------------------------------
-- Communication with Server
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Access Context
-------------------------------------------------------------------------------

pull :: (HasInChan env) => Neovim env InMessage
pull = liftIO . atomically . readTChan =<< view #inChan

push :: (HasOutChan env, J.ToJSON a, Show a) => a -> Neovim env ()
push x = do
    outCh <- view #outChan
    liftIO $ atomically $ writeTChan outCh $ toStrictBytes (J.encode x)

modifyReadContext :: (MonadReader env m, MonadIO m, HasContext env)
                  => (Context -> Context) -> (Context -> a) -> m a
modifyReadContext modifier reader = do
    ctxV <- view #context
    ctx <- atomically $ do
      ctx <- readTVar ctxV
      writeTVar ctxV $! modifier ctx
      return ctx
    return $ reader ctx

---

modifyContext :: (MonadReader env m, MonadIO m, HasContext env)
               => (Context -> Context) -> m ()
modifyContext modifier = modifyReadContext modifier (const ())

readContext :: (MonadReader env m, MonadIO m, HasContext env)
            => (Context -> a) -> m a
readContext reader = modifyReadContext id reader

---

genUniqueID :: (MonadReader env m, MonadIO m, HasContext env) => m ID
genUniqueID = modifyReadContext
    (#uniqueID %~ (+1))
    (views #uniqueID (IDNum . fromIntegral))

genUniqueVersion :: (MonadReader env m, MonadIO m, HasContext env)
                 => m Version
genUniqueVersion = modifyReadContext
    (#uniqueVersion %~ (+1))
    (view #uniqueVersion)

addIdMethodMap :: (MonadReader env m, MonadIO m, HasContext env)
               => ID -> ClientRequestMethod -> m ()
addIdMethodMap id' m = modifyContext (#idMethodMap %~ M.insert id' m)

registerCallback :: (MonadReader env m, MonadIO m, HasContext env)
                 => ID -> Callback -> m ()
registerCallback id' callback = modifyContext (#callbacks %~ M.insert id' callback)

getCallback :: (MonadReader env m, MonadIO m, HasContext env)
            => ID -> m (Maybe Callback)
getCallback id' = readContext $ views #callbacks (M.lookup id')

removeCallback :: (MonadReader env m, MonadIO m, HasContext env)
               => ID -> m ()
removeCallback id' = modifyContext $ #callbacks %~ M.delete id'

-------------------------------------------------------------------------------
-- Dispatcher
-------------------------------------------------------------------------------

asyncNeovim :: NFData a => iEnv -> Neovim iEnv a -> Neovim env (Async a)
asyncNeovim r a = async $ retypeEnvNeovim (const r) a

dispatch :: [Plugin] -> NeovimLsp ()
dispatch hs = do
    inChG   <- view #inChan
    outCh   <- view #outChan
    ctx     <- view #context
    logFunc <- view #logFunc

    inChs <- forM hs $ \(Plugin name action) -> do
      inCh <- liftIO newTChanIO
      let pluginEnv = #inChan  @= inCh
                   <! #outChan @= outCh
                   <! #context @= ctx
                   <! #logFunc @= logFunc
                   <! nil
      a <- asyncNeovim pluginEnv $ loggingErrorImmortal action
      registerAsyncHandle name a
      return inCh

    dispatcher <- async $ forever $ loggingErrorImmortal $ do
      rawInput <- atomically (readTChan inChG)
      let Just !v = J.decode (fromStrictBytes rawInput)
      idMethodMap <- view #idMethodMap <$> readTVarIO ctx
      case parseMessage idMethodMap v of
        Right !msg -> do
            logInfo $ displayShow (methodOf msg)
            forM_ inChs $ atomically . flip writeTChan msg
        Left e -> logError $ fromString $ init $ unlines
            [ "dispatcher: could not parse input."
            , "error: " ++ e
            ]
    registerAsyncHandle "dispatcher" dispatcher

registerAsyncHandle :: String -> Async () -> NeovimLsp ()
registerAsyncHandle name a =
    #otherHandles %== (\(OtherHandles hs) -> OtherHandles ((name,a):hs))

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

loggingErrorImmortal
  :: (HasCallStack, MonadReader r m, MonadUnliftIO m, HasLogFunc r)
  => m () -> m ()
loggingErrorImmortal = handleAnyDeep $ \e -> logError (displayShow e)
  where ?callstack = popCallStack callStack

loggingError
  :: (HasCallStack, MonadReader r m, MonadUnliftIO m, HasLogFunc r)
  => m a -> m a
loggingError = handleAny $ \e -> logError (displayShow e) >> throwIO e
  where ?callstack = popCallStack callStack

loggingErrorDeep
  :: (HasCallStack, MonadReader r m, MonadUnliftIO m, HasLogFunc r, NFData a)
  => m a -> m a
loggingErrorDeep = handleAnyDeep $ \e -> logError (displayShow e) >> throwIO e
  where ?callstack = popCallStack callStack

catchAndDisplay
  :: HasLogFunc env
  => Neovim env () -> Neovim env ()
catchAndDisplay = handleAnyDeep $ \e -> logError (displayShow e) >> vim_report_error' (show e)

retypeEnvNeovim :: (env -> env') -> Neovim env' a -> Neovim env a
retypeEnvNeovim f (Internal.Neovim m) =
  Internal.Neovim $
    transResourceT
      (withReaderT $ \cfg -> Internal.retypeConfig (f (Internal.customConfig cfg)) cfg)
      m

