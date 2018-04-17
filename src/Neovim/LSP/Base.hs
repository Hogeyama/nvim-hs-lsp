
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}
{-# OPTIONS_GHC -Wall               #-}


module Neovim.LSP.Base where

import           Control.DeepSeq            (NFData)
import           Control.Lens               hiding (Context)
import           Control.Monad              (forM, forM_, forever)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class (MonadReader, ask)
import qualified Data.Aeson                 as J
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Constraint            (withDict)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (isPrefixOf)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (isJust)
import           Data.Singletons            (Sing, SomeSing (..), fromSing,
                                             singByProxy, toSing)
import           System.Exit                (exitSuccess)
import           System.IO                  (hGetLine)
import           System.IO.Error            (isEOFError)
import           System.Log.Formatter       (simpleLogFormatter)
import           System.Log.Handler         (setFormatter)
import           System.Log.Handler.Simple  (fileHandler)
import qualified System.Log.Logger          as L
import           System.Process             (CreateProcess (..), ProcessHandle,
                                             StdStream (..), createProcess,
                                             proc, terminateProcess)
import           UnliftIO

import           Neovim                     hiding (Plugin)
import qualified Neovim.Context.Internal    as Internal
import           Neovim.LSP.Protocol.Type


-------------------------------------------------------------------------------
-- Env
-------------------------------------------------------------------------------

type NeovimLsp = Neovim LspEnv

data LspEnv = LspEnv
  { _lspEnvServerHandles :: !(TVar (Maybe ServerHandles))
  , _lspEnvFileType      :: !(TVar (Maybe String))
  , _lspEnvOtherHandles  :: !(TVar OtherHandles)
  ,  lspEnvInChan        :: !(TChan B.ByteString)
  , _lspEnvOutChan       :: !(TChan B.ByteString)
  , _lspEnvOpenedFiles   :: !(TVar (Map Uri Version))
  , _lspEnvContext       :: !(TVar Context)
  , _lspEnvLoggerName    :: !String
  , _lspEnvOtherState    :: !OtherState
  }

initialEnvM :: MonadIO m => m LspEnv
initialEnvM = liftIO $ do
  _lspEnvServerHandles <- newTVarIO Nothing
  _lspEnvOtherHandles  <- newTVarIO (OtherHandles [])
  lspEnvInChan         <- newTChanIO
  _lspEnvOutChan       <- newTChanIO
  _lspEnvOpenedFiles   <- newTVarIO M.empty
  _lspEnvContext       <- newTVarIO initialContext
  _lspEnvLoggerName    <- return topLoggerName
  _lspEnvOtherState    <- return OtherState
  _lspEnvFileType      <- newTVarIO Nothing
  return LspEnv {..}

topLoggerName :: String
topLoggerName = "nvim-hs-lsp"

senderLoggerName :: String
senderLoggerName = topLoggerName ++ "." ++ "send"

receiverLoggerName :: String
receiverLoggerName = topLoggerName ++ "." ++ "receive"

data PluginEnv = PluginEnv
  { _pluginEnvInChan     :: !(TChan InMessage)
  , _pluginEnvOutChan    :: !(TChan ByteString)
  , _pluginEnvContext    :: !(TVar Context)
  , _pluginEnvLoggerName :: !String
  }

data Context = Context
  { _lspIdMap         :: !(Map ID ClientRequestMethod)
  , _lspUniqueID      :: !Int
  , _lspUniqueVersion :: !Version
  , _lspVersionMap    :: !(Map Uri Version)
  , _lspCallbacks     :: !(Map ID Callback)
  }

data Callback where
  Callback :: Typeable m => TMVar a -> CallbackOf m a -> Callback
type CallbackOf m a = ServerResponse m -> PluginAction a

initialContext :: Context
initialContext = Context
  { _lspIdMap         = M.empty
  , _lspUniqueID      = 0
  , _lspUniqueVersion = 0
  , _lspVersionMap    = M.empty
  , _lspCallbacks     = M.empty
  }

data ServerHandles = ServerHandles
  { serverIn  :: !Handle
  , serverOut :: !Handle
  , serverErr :: !Handle
  , serverPH  :: !ProcessHandle
  }

-- sender, receiver, watcher of serverErr, dispatcher, plugins
newtype OtherHandles = OtherHandles
  { unOtherHandles :: [(String, Async ())] } -- (name,_)

data OtherState = OtherState
  {  }

-------------------------------------------------------------------------------
-- Plugin
-------------------------------------------------------------------------------

type PluginAction = Neovim PluginEnv

data Plugin = Plugin
  { pluginName   :: !String
  , pluginAction :: !(PluginAction ())
  }

data InMessage where
  SomeNoti :: ImplNotification m => !(ServerNotification m) -> InMessage
  SomeReq  :: ImplRequest      m => !(ServerRequest      m) -> InMessage
  SomeResp :: ImplResponse     m => !(ServerResponse     m) -> InMessage
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
makeLenses ''Context
makeLensesWith camelCaseFields ''LspEnv
makeLensesWith camelCaseFields ''PluginEnv
makeLensesWith camelCaseFields ''OtherState

type HasInChan'        env = HasInChan        env (TChan InMessage)
type HasOutChan'       env = HasOutChan       env (TChan B.ByteString)
type HasContext'       env = HasContext       env (TVar  Context)
type HasOpenedFiles'   env = HasOpenedFiles   env (TVar  (Map Uri Version))
type HasOtherHandles'  env = HasOtherHandles  env (TVar  OtherHandles)
type HasServerHandles' env = HasServerHandles env (TVar  (Maybe ServerHandles))
type HasLoggerName'    env = HasLoggerName    env String

useTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> m a
useTV getter = liftIO . readTVarIO =<< view getter

usesTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> (a -> b) -> m b
usesTV getter f = f <$> useTV getter

assignTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> a -> m ()
assignTV getter x = do
  v <- view getter
  liftIO $ atomically $ writeTVar v x

modifyOverTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> (a -> a) -> m ()
modifyOverTV getter f = do
  v <- view getter
  liftIO $ atomically $ modifyTVar' v f

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
  serverHandles .== Just ServerHandles
        { serverIn  = hin
        , serverOut = hout
        , serverErr = herr
        , serverPH  = ph
        }
  liftIO $ do
    hSetBuffering hin  NoBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering
    setupLogger
  inCh  <- asks lspEnvInChan
  outCh <- asks _lspEnvOutChan
  registerAsyncHandle "sender" =<< async (liftIO (sender hin outCh))
  registerAsyncHandle "receiver" =<< async (liftIO (receiver hout inCh))
  registerAsyncHandle "herr-watcher" =<< async (watcher herr)
  infoM $ unlines
      [ ""
      , "＿人人人人人人＿"
      , "＞　__init__　＜"
      , "￣Y^Y^Y^Y^Y^Y^Y￣"
      ]
  where
    watcher :: Handle -> Neovim env ()
    watcher herr = forever $ catchIO
      (liftIO (hGetLine herr) >>= showAndLog . ("STDERR: "++))
      (\e ->  if isEOFError e
              then throwIO e
              else showAndLog $ "STDERR: Exception: " ++ show e)

    showAndLog :: String -> Neovim env ()
    showAndLog msg = do
        liftIO $ L.errorM topLoggerName msg
        vim_report_error' msg

isInitialized :: NeovimLsp Bool
isInitialized = usesTV serverHandles isJust

uninitializedError :: a
uninitializedError = error "not initialized (TODO: fabulous message)"

registerAsyncHandle :: (MonadReader env m, MonadIO m, HasOtherHandles' env)
                    => String -> Async () -> m ()
registerAsyncHandle name a =
  otherHandles %== (\(OtherHandles hs) -> OtherHandles ((name,a):hs))


-------------------------------------------------------------------------------
-- Finalize
-------------------------------------------------------------------------------

finalizeLSP :: NeovimLsp ()
finalizeLSP = do
  mapM_ (cancel.snd) =<< usesTV otherHandles unOtherHandles
  useTV serverHandles >>= \case
    Nothing -> return ()
    Just sh -> liftIO $ terminateProcess (serverPH sh)
  liftIO L.removeAllHandlers

-------------------------------------------------------------------------------
-- Logger
-------------------------------------------------------------------------------

setupLogger :: IO ()
setupLogger = do
  L.updateGlobalLogger L.rootLoggerName L.removeHandler
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
  --L.updateGlobalLogger senderLoggerName   (L.setLevel L.WARNING)
  --L.updateGlobalLogger receiverLoggerName (L.setLevel L.WARNING)
  h <- do lh <- fileHandler "/tmp/nvim-hs-lsp.log" L.DEBUG
          return $ setFormatter
                      lh
                      (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  L.updateGlobalLogger topLoggerName (L.addHandler h)

debugM :: (MonadReader r m, MonadIO m, HasLoggerName' r) => String -> m ()
debugM = withLoggerName L.debugM

warningM :: (MonadReader r m, MonadIO m, HasLoggerName' r) => String -> m ()
warningM = withLoggerName L.warningM

errorM :: (MonadReader r m, MonadIO m, HasLoggerName' r) => String -> m ()
errorM = withLoggerName L.errorM

infoM :: (MonadReader r m, MonadIO m, HasLoggerName' r) => String -> m ()
infoM = withLoggerName L.infoM

withLoggerName :: (MonadReader r m, MonadIO m, HasLoggerName' r)
               => (String -> String -> IO ()) -> String -> m ()
withLoggerName logger s = view loggerName >>= liftIO . (logger `flip` s)

setLogLevel :: (MonadReader r m, MonadIO m, HasLoggerName' r)
            => L.Priority -> m ()
setLogLevel p = view loggerName >>= liftIO . (L.updateGlobalLogger `flip` L.setLevel p)


-------------------------------------------------------------------------------
-- Communication with Server
-------------------------------------------------------------------------------

sender :: Handle -> TChan B.ByteString -> IO ()
sender serverIn chan = forever $ do
  bs <- atomically $ readTChan chan
  B.hPutStr serverIn $ B.concat
      [ "Content-Length: "
      , B.pack $ show $ B.length bs
      , "\r\n\r\n"
      , bs
      ]
  L.debugM senderLoggerName $ "=> " ++ B.unpack bs

receiver :: Handle -> TChan B.ByteString -> IO ()
receiver serverOut chan = forever $ do
    v <- receive serverOut
    atomically $ writeTChan chan v
  where
    receive :: Handle -> IO B.ByteString
    receive h = do
      len <- contentLength <$> readHeader h
      content <- B.hGet h len
      L.debugM receiverLoggerName $ "<= " ++ B.unpack content
      return content

    readHeader :: Handle -> IO Header
    readHeader h = go Header { contentLength = Prelude.error "broken header"
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
             | otherwise          -> Prelude.error $ "unknown input: " ++ show x

data Header = Header
  { contentLength :: Int -- lazy initialization
  , contentType   :: !(Maybe String)
  } deriving (Eq, Show)


-------------------------------------------------------------------------------
-- Access Context
-------------------------------------------------------------------------------

pull :: (HasInChan' env) => Neovim env InMessage
pull = liftIO . atomically . readTChan =<< view inChan

push :: (HasOutChan' env, J.ToJSON a, Show a) => a -> Neovim env ()
push x = do
  outCh <- view outChan
  liftIO $ atomically $ writeTChan outCh $ J.encode x

modifyReadContext :: (MonadReader env m, MonadIO m, HasContext' env)
              => (Context -> Context) -> (Context -> a) -> m a
modifyReadContext modifier reader = do
  ctxV <- view context
  ctx <- atomically $ do
    ctx <- readTVar ctxV
    writeTVar ctxV $! modifier ctx
    return ctx
  return $ reader ctx

---

modifyContext :: (MonadReader env m, MonadIO m, HasContext' env)
               => (Context -> Context) -> m ()
modifyContext modifier = modifyReadContext modifier (const ())

readContext :: (MonadReader env m, MonadIO m, HasContext' env)
            => (Context -> a) -> m a
readContext reader = modifyReadContext id reader

---

genUniqueID :: (MonadReader env m, MonadIO m, HasContext' env) => m ID
genUniqueID = modifyReadContext
  (lspUniqueID %~ succ)
  (views lspUniqueID (IDNum . fromIntegral))

genUniqueVersion :: (MonadReader env m, MonadIO m, HasContext' env)
                 => m Version
genUniqueVersion = modifyReadContext
  (lspUniqueVersion %~ succ)
  (view lspUniqueVersion)

addIdMethodMap :: (MonadReader env m, MonadIO m, HasContext' env)
               => ID -> ClientRequestMethod -> m ()
addIdMethodMap id' m = modifyContext (lspIdMap %~ M.insert id' m)

registerCallback :: (MonadReader env m, MonadIO m, HasContext' env)
                 => ID -> Callback -> m ()
registerCallback id' callback = modifyContext (lspCallbacks %~ M.insert id' callback)

getCallback :: (MonadReader env m, MonadIO m, HasContext' env)
            => ID -> m (Maybe Callback)
getCallback id' = readContext (views lspCallbacks (M.lookup id'))

removeCallback :: (MonadReader env m, MonadIO m, HasContext' env)
               => ID -> m ()
removeCallback id' = modifyContext (lspCallbacks %~ M.delete id')

-------------------------------------------------------------------------------
-- Dispatcher
-------------------------------------------------------------------------------

-- TODO
asyncNeovim :: NFData a => iEnv -> Neovim iEnv a -> Neovim env (Async ())
asyncNeovim r a = do
    cfg <- Internal.ask'
    let threadConfig = Internal.retypeConfig r cfg
    liftIO . async . void $ Internal.runNeovim threadConfig a

dispatch :: [Plugin] -> NeovimLsp ()
dispatch hs = do
    LspEnv {  lspEnvInChan  = inChG
           , _lspEnvOutChan = outCh
           , _lspEnvContext = ctx
           } <- ask

    inChs <- forM hs $ \(Plugin name action) -> do
      inCh <- liftIO newTChanIO
      let config = PluginEnv
                   { _pluginEnvInChan     = inCh
                   , _pluginEnvOutChan    = outCh
                   , _pluginEnvContext    = ctx
                   , _pluginEnvLoggerName = topLoggerName ++ "." ++ name
                   }
      a <- asyncNeovim config $ loggingError action
      registerAsyncHandle name a
      return inCh

    dispatcher <- async $ loggingError $ forever $ do
        rawInput <- atomically (readTChan inChG)
        let Just !v = J.decode rawInput
        idMap <- view lspIdMap <$> readTVarIO ctx
        case toInMessage idMap v of
          Right !msg -> forM_ inChs $ atomically . flip writeTChan msg
          Left e -> errorM $ init $ unlines
              [ "dispatcher: could not parse input."
              , "input: " ++ B.unpack rawInput
              , "error: " ++ e
              ]
    registerAsyncHandle "dispatcher" dispatcher

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

loggingError :: (MonadReader r m, MonadUnliftIO m, HasLoggerName' r)
             => m a -> m a
loggingError = handleAny $ \e -> errorM (show e) >> throwIO e

