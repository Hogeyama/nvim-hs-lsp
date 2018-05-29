
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Neovim.LSP.Base where

import           RIO
import qualified RIO.ByteString             as B
import qualified RIO.Text                   as T
import           RIO.List.Partial           (init)
import           Prelude                    (read, succ)

import           Control.Lens               (makeLenses, views)
import           Control.Lens.Operators
import           Control.Monad              ((>=>), forM, forM_, forever)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.Aeson                 as J
import           Data.Extensible
import           Data.Constraint            (withDict)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (isPrefixOf)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (isJust)
import           Data.Singletons            (Sing, SomeSing (..), fromSing,
                                             singByProxy, toSing)
import           GHC.TypeLits               (Symbol)
import           System.Exit                (exitSuccess)
import           System.IO                  (IOMode(..), hGetLine, openFile)
import           System.IO.Error            (isEOFError)
import           System.Process             (CreateProcess (..), ProcessHandle,
                                             StdStream (..), createProcess,
                                             proc, terminateProcess)

import           Neovim                     hiding (Plugin, (<>))
import qualified Neovim.Context.Internal    as Internal
import           Neovim.LSP.Protocol.Type

-------------------------------------------------------------------------------
-- Env
-------------------------------------------------------------------------------

type NeovimLsp = Neovim LspEnv

-- | Enviroment of the main thread.
type LspEnv = Record
  '[ "serverHandles" >: TVar (Maybe ServerHandles)
   , "logFileHandle" >: Handle
   , "fileType"      >: TVar (Maybe String)
   , "otherHandles"  >: TVar OtherHandles
   , "inChan"        >: TChan ByteString
   , "outChan"       >: TChan ByteString
   , "openedFiles"   >: TVar (Map Uri Version)
   , "context"       >: TVar Context
   , "logFunc"       >: LogFunc
   ]

initialEnvM :: MonadIO m => m LspEnv
initialEnvM = do
  h <- liftIO $ openFile "/tmp/nvim-hs-lsp.log" AppendMode
  hsequence $ #serverHandles <@=> newTVarIO Nothing
           <! #logFileHandle <@=> return h
           <! #fileType      <@=> newTVarIO Nothing
           <! #otherHandles  <@=> newTVarIO (OtherHandles [])
           <! #inChan        <@=> newTChanIO
           <! #outChan       <@=> newTChanIO
           <! #openedFiles   <@=> newTVarIO M.empty
           <! #context       <@=> newTVarIO initialContext
           <! #logFunc       <@=> makeLogFunc h
           <! nil
  where
    makeLogFunc h = liftIO $ do
      hSetBuffering h LineBuffering
      opts <- setLogTerminal False .
              setLogUseColor False <$>
              logOptionsHandle h True
      withLogFunc opts return
      -- TODO 'withLogFunc opts return'は普通にまずいけど
      --      'setLogTerminal False'しておけば
      --      リソースの取得はしないので大丈夫そう

-- | Enviroment of each plugin.
type PluginEnv = Record
  '[ "inChan"  >: TChan InMessage
   , "outChan" >: TChan ByteString
   , "context" >: TVar Context
   , "logFunc" >: LogFunc
   ]

-- | Context shared with main thread and all plugins.
type Context = Record
  '[ "idMethodMap"   >: Map ID ClientRequestMethod
   , "uniqueID"      >: Int
   , "uniqueVersion" >: Version
   , "versionMap"    >: Map Uri Version
   , "callbacks"     >: Map ID Callback
   , "lspConfig"     >: LspConfig
   , "otherState"    >: OtherState
   ]

data Callback where
  Callback :: Typeable m => TMVar a -> CallbackOf m a -> Callback
type CallbackOf m a = ServerResponse m -> PluginAction a

initialContext :: Context
initialContext =
     #idMethodMap   @= M.empty
  <! #uniqueID      @= 0
  <! #uniqueVersion @= 0
  <! #versionMap    @= M.empty
  <! #callbacks     @= M.empty
  <! #lspConfig     @= LspConfig
                     { _autoLoadQuickfix = False
                     }
  <! #otherState    @= OtherState
                     { _diagnosticsMap = M.empty
                     }
  <! nil

data ServerHandles = ServerHandles
  { serverIn  :: Handle
  , serverOut :: Handle
  , serverErr :: Handle
  , serverPH  :: ProcessHandle
  }

-- sender, receiver, watcher of serverErr, dispatcher, plugins
newtype OtherHandles = OtherHandles
  { unOtherHandles :: [(String, Async ())] } -- (name,_)

data OtherState = OtherState
  { _diagnosticsMap :: Map Uri [Diagnostic]
  }

data LspConfig = LspConfig
  { _autoLoadQuickfix :: Bool
  }

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

methodOf :: InMessage -> InMessageMethod
methodOf (SomeNoti noti) = InNoti $ fromSing $ singByProxy noti
methodOf (SomeReq  req ) = InReq  $ fromSing $ singByProxy req
methodOf (SomeResp resp) = InResp $ fromSing $ singByProxy resp

toInMessage :: Map ID ClientRequestMethod -> J.Value -> Either String InMessage
toInMessage map' v@(J.Object o) = mmethod >>= \case
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
toInMessage _ _ = Left "そんなバナナ2"

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

type family UnRecord env :: [Assoc Symbol *] where UnRecord (Record xs) = xs
type IsRealRecord env = Record (UnRecord env) ~ env
type Associate' k v env = (IsRealRecord env, Associate k v (UnRecord env))
type HasContext env = Associate' "context" (TVar Context)     env
type HasInChan  env = Associate' "inChan"  (TChan InMessage)  env
type HasOutChan env = Associate' "outChan" (TChan ByteString) env
instance Associate "logFunc" LogFunc xs
  => HasLogFunc (Record xs) where logFuncL = #logFunc
-- #context :: HasContext env => Lens' env (TVar Context)

{-
-- ExtensibleのIsRecordが使えるかも
instance IsRecord (Record (xs :: [Assoc Symbol *])) where
  type RecFields (Record xs) = xs
  recordToList = toHList
  recordFromList = fromHList
type HasContext env = (IsRecord env, Associate "context" (TVar Context)     (RecFields env))
type HasInChan  env = (IsRecord env, Associate "inChan"  (TChan InMessage)  (RecFields env))
type HasOutChan env = (IsRecord env, Associate "outChan" (TChan ByteString) (RecFields env))
-- (record . #context) :: HasContext env => Lens' env (TVar Context)
-}

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
-- Initialize
-------------------------------------------------------------------------------

initializeLsp :: String -> [String] -> NeovimLsp ()
initializeLsp cmd args = do
    (Just hin, Just hout, Just herr, ph) <-
        liftIO $ createProcess $ (proc cmd args)
          { cwd     = Nothing
          , std_in  = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
    #serverHandles .== Just ServerHandles
          { serverIn  = hin
          , serverOut = hout
          , serverErr = herr
          , serverPH  = ph
          }
    liftIO $ do
      hSetBuffering hin  NoBuffering
      hSetBuffering hout NoBuffering
      hSetBuffering herr NoBuffering
    inCh <- view #inChan
    outCh <- view #outChan
    registerAsyncHandle "sender" =<< async (sender hin outCh)
    registerAsyncHandle "receiver" =<< async (receiver hout inCh)
    registerAsyncHandle "herr-watcher" =<< async (watcher herr)
    loadLspConfig
    logInfo $ fromString $ unlines
        [ ""
        , "＿人人人人人人＿"
        , "＞　__init__　＜"
        , "￣Y^Y^Y^Y^Y^Y^Y￣"
        ]
  where
    watcher herr = forever $
        do s <- B.hGetLine herr
           showAndLog $ "STDERR: " <> displayBytesUtf8 s
      `catchIO` \e ->
        if isEOFError e
        then throwIO e
        else showAndLog $ "STDERR: Exception: " <> displayShow e

    showAndLog msg = do
        logError msg
        vim_report_error' $ T.unpack $ utf8BuilderToText msg
        -- なんでRIOには'Utf8Builder -> String'の関数がないんだ

loadLspConfig :: HasContext env => Neovim env ()
loadLspConfig = do
    before <- readContext $ view #lspConfig
    after <- updateLspConfig before
    modifyContext $ #lspConfig .~ after

updateLspConfig :: LspConfig -> Neovim env LspConfig
updateLspConfig =
    update "NvimHsLsp_autoLoadQuickfix" (\o -> autoLoadQuickfix .~ toBool o) >=>
    return
  where
    update var f before = vim_get_var var >>= \case
      Right o -> return $ f o before
      Left _ -> return before

    toBool (fromObject @Bool -> Right b) = b
    toBool (fromObject @Int  -> Right 0) = False
    toBool _ = True

isInitialized :: NeovimLsp Bool
isInitialized = usesTV #serverHandles isJust

uninitializedError :: a
uninitializedError = error "not initialized (TODO: fabulous message)"

registerAsyncHandle :: String -> Async () -> NeovimLsp ()
registerAsyncHandle name a =
    #otherHandles %== (\(OtherHandles hs) -> OtherHandles ((name,a):hs))

-------------------------------------------------------------------------------
-- Finalize
-------------------------------------------------------------------------------

finalizeLSP :: NeovimLsp ()
finalizeLSP = do
    mapM_ (cancel.snd) =<< usesTV #otherHandles unOtherHandles
    useTV #serverHandles >>= \case
      Nothing -> return ()
      Just sh -> liftIO $ terminateProcess (serverPH sh)
    hClose =<< view #logFileHandle

-------------------------------------------------------------------------------
-- Communication with Server
-------------------------------------------------------------------------------

sender :: (MonadIO m, MonadReader env m, HasLogFunc env)
       => Handle -> TChan ByteString -> m ()
sender serverIn chan = forever $ do
    bs <- atomically $ readTChan chan
    hPutBuilder serverIn $ getUtf8Builder $ mconcat
        [ "Content-Length: "
        , displayShow $ B.length bs
        , "\r\n\r\n"
        , displayBytesUtf8 bs
        ]
    logDebug $ "=> " <> displayBytesUtf8 bs

receiver :: (MonadIO m, MonadReader env m, HasLogFunc env)
         => Handle -> TChan ByteString -> m ()
receiver serverOut chan = forever $ do
    v <- receive serverOut
    atomically $ writeTChan chan v
  where
    receive h = do
        len <- liftIO $ contentLength <$> readHeader h
        content <- B.hGet h len
        logDebug $ "<= " <> displayBytesUtf8 content
        return content

    readHeader h = go Header { contentLength = error "broken header"
                             , contentType   = Nothing }
      where
        go header = do
          x <- hGetLine h
          if | "Content-Length: " `isPrefixOf` x ->
                  go $ header { contentLength = read (drop 16 x) }
             | "Content-Type: " `isPrefixOf` x ->
                  go $ header { contentType = Just (drop 14 x) }
             | "\r" == x          -> return header
             | "ExitSuccess" == x -> exitSuccess -- TODO hieだけだよね，これ
             | otherwise          -> error $ "unknown input: " ++ show x

data Header = Header
  { contentLength :: ~Int -- lazy initialization
  , contentType   :: Maybe String
  } deriving (Eq, Show)


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
    (#uniqueID %~ succ)
    (views #uniqueID (IDNum . fromIntegral))

genUniqueVersion :: (MonadReader env m, MonadIO m, HasContext env)
                 => m Version
genUniqueVersion = modifyReadContext
    (#uniqueVersion %~ succ)
    (view #uniqueVersion)

addIdMethodMap :: (MonadReader env m, MonadIO m, HasContext env)
               => ID -> ClientRequestMethod -> m ()
addIdMethodMap id' m = modifyContext (#idMethodMap %~ M.insert id' m)

registerCallback :: (MonadReader env m, MonadIO m, HasContext env)
                 => ID -> Callback -> m ()
registerCallback id' callback = modifyContext (#callbacks %~ M.insert id' callback)

getCallback :: (MonadReader env m, MonadIO m, HasContext env)
            => ID -> m (Maybe Callback)
getCallback id' = readContext (views #callbacks (M.lookup id'))

removeCallback :: (MonadReader env m, MonadIO m, HasContext env)
               => ID -> m ()
removeCallback id' = modifyContext (#callbacks %~ M.delete id')

-------------------------------------------------------------------------------
-- Dispatcher
-------------------------------------------------------------------------------

asyncNeovim :: NFData a => iEnv -> Neovim iEnv a -> Neovim env (Async ())
asyncNeovim r a = do
    cfg <- Internal.ask'
    let threadConfig = Internal.retypeConfig r cfg
    liftIO . async . void $ Internal.runNeovim threadConfig a

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
      a <- asyncNeovim pluginEnv $ loggingError action
      registerAsyncHandle name a
      return inCh

    dispatcher <- async $ loggingError $ forever $ do
      rawInput <- atomically (readTChan inChG)
      let Just !v = J.decode (fromStrictBytes rawInput)
      idMethodMap <- view #idMethodMap <$> readTVarIO ctx
      case toInMessage idMethodMap v of
        Right !msg -> forM_ inChs $ atomically . flip writeTChan msg
        Left e -> logError $ fromString $ init $ unlines
            [ "dispatcher: could not parse input."
            --, "input: " ++ B.unpack rawInput
            , "error: " ++ e
            ]
    registerAsyncHandle "dispatcher" dispatcher

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

loggingError :: (MonadReader r m, MonadUnliftIO m, HasLogFunc r)
             => m a -> m a
loggingError = handleAny $ \e -> logError (displayShow e) >> throwIO e

