
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Neovim.LSP.Base where

import           RIO
import qualified RIO.HashMap                  as HM
import           RIO.List.Partial             (init)
import qualified RIO.Map                      as M

import           Control.Lens                 (makeLenses, views)
import           Control.Lens.Operators
import           Control.Monad                (forM, forM_, forever)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (withReaderT)
import           Control.Monad.Trans.Resource (transResourceT)
import qualified Data.Aeson                   as J
import           Data.Constraint              (withDict)
import           Data.Extensible.Rexport
import           Data.Singletons              (Sing, SomeSing (..), fromSing,
                                               singByProxy, toSing)
import           GHC.Stack                    (callStack, popCallStack)
import           System.IO                    (openFile)
import           System.Process               (ProcessHandle)

import           Neovim                       hiding (Plugin, (<>))
import qualified Neovim.Context.Internal      as Internal
import           Neovim.LSP.Protocol.Type

-------------------------------------------------------------------------------
-- Env
-------------------------------------------------------------------------------

type NeovimLsp = Neovim LspEnv

-- | Enviroment of the main thread.
type LspEnv = OrigRecord
  '[ "logFunc"          >: LogFunc
   , "logFileHandle"    >: Handle
   , "logFuncFinalizer" >: IO ()
   , "languageMap"      >: TVar (Map Language LanguageEnv)
   ]

type LanguageEnv = OrigRecord
  '[ "logFunc"          >: LogFunc
   , "language"         >: Language
   , "serverHandles"    >: ServerHandles
   , "otherHandles"     >: TVar OtherHandles
   , "inChan"           >: TChan ByteString
   , "outChan"          >: TChan ByteString
   , "context"          >: TVar Context
   ]

initialEnvM :: MonadIO m => m LspEnv
initialEnvM = do
  h <- liftIO $ openFile "/tmp/nvim-hs-lsp.log" AppendMode
  (lf, lfFinalizer) <- liftIO $ makeLogFunc h
  hsequence $ #logFunc          <@=> return lf
           <! #logFileHandle    <@=> return h
           <! #logFuncFinalizer <@=> return lfFinalizer
           <! #languageMap      <@=> newTVarIO M.empty
           <! nil
  where
    makeLogFunc h = do
        hSetBuffering h LineBuffering
        opts <- setLogTerminal False .
                setLogUseColor False <$>
                logOptionsHandle h True
        newLogFunc opts

-- | Enviroment of each plugin.
type WorkerEnv = OrigRecord
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
type CallbackOf m a = ServerResponse m -> WorkerAction a

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
  { serverIn         :: Handle
  , serverOut        :: Handle
  , serverErr        :: Handle
  , serverProcHandle :: ProcessHandle
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
  , _settingsPath     :: Maybe FilePath
  , _lspCommands      :: Map Language [String]
  }
defaultLspConfig :: LspConfig
defaultLspConfig =  LspConfig
  { _autoLoadQuickfix = False
  , _settingsPath = Nothing
  , _lspCommands = M.empty
  }

type Language = Text

-------------------------------------------------------------------------------
-- Worker
-------------------------------------------------------------------------------

type WorkerAction = Neovim WorkerEnv

data Worker = Worker
  { pluginName   :: String
  , pluginAction :: WorkerAction ()
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

instance Associate "logFunc" LogFunc xs
  => HasLogFunc (OrigRecord xs) where logFuncL = #logFunc

class HasContext env where
  contextL :: Lens' env (TVar Context)
instance Associate "context" (TVar Context) xs => HasContext (OrigRecord xs) where
  contextL = #context

class HasInChan env where
  inChanL :: Lens' env (TChan InMessage)
instance Associate "inChan" (TChan InMessage) xs => HasInChan (OrigRecord xs) where
  inChanL = #inChan

class HasOutChan env where
  outChanL :: Lens' env (TChan ByteString)
instance Associate "outChan" (TChan ByteString) xs => HasOutChan (OrigRecord xs) where
  outChanL = #outChan

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
-- Access Context
-------------------------------------------------------------------------------

pull :: (HasInChan env, MonadReader env m, MonadIO m)
     => m InMessage
pull = liftIO . atomically . readTChan =<< view inChanL

push :: (HasOutChan env, J.ToJSON a, Show a, MonadReader env m, MonadIO m) 
     => a -> m ()
push x = do
    outCh <- view outChanL
    liftIO $ atomically $ writeTChan outCh $ toStrictBytes (J.encode x)

modifyReadContext :: (MonadReader env m, MonadIO m, HasContext env)
                  => (Context -> (Context, a)) -> m a
modifyReadContext f = do
    ctxV <- view contextL
    atomically $ do
      ctx <- readTVar ctxV
      let (newCtx, x) = f ctx
      writeTVar ctxV $! newCtx
      return x

---

modifyContext :: (MonadReader env m, MonadIO m, HasContext env)
               => (Context -> Context) -> m ()
modifyContext f = modifyReadContext (f &&& const ())

readContext :: (MonadReader env m, MonadIO m, HasContext env)
            => (Context -> a) -> m a
readContext f = do
    ctxV <- view contextL
    f <$> readTVarIO ctxV

---

genUniqueID :: (MonadReader env m, MonadIO m, HasContext env) => m ID
genUniqueID = modifyReadContext $
    #uniqueID %~ (+1) &&& views #uniqueID (IDNum . fromIntegral)

genUniqueVersion :: (MonadReader env m, MonadIO m, HasContext env)
                 => m Version
genUniqueVersion = modifyReadContext $
    #uniqueVersion %~ (+1) &&& view #uniqueVersion

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

-- focusLangは外で使った方が良いか
dispatch :: [Worker] -> Neovim LanguageEnv ()
dispatch hs = do
    inChG   <- view #inChan
    outCh   <- view #outChan
    ctx     <- view #context
    logFunc <- view #logFunc

    inChs <- forM hs $ \(Worker name action) -> do
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

registerAsyncHandle :: String -> Async () -> Neovim LanguageEnv ()
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

focusLang :: Language -> Neovim LanguageEnv a -> Neovim LspEnv (Maybe a)
focusLang lang m = usesTV #languageMap (M.lookup lang) >>= \case
    Nothing -> return Nothing
    Just x -> Just <$> retypeEnvNeovim (const x) m

