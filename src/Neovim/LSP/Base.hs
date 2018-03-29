
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

import           UnliftIO
import           Control.DeepSeq            (NFData)
import           Control.Lens               hiding (Context)
import           Control.Monad              (forM, forM_, forever, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class (MonadReader, ask)
import qualified Data.Aeson                 as J
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Constraint            (withDict)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (isPrefixOf)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (isJust)
import           Data.Singletons            (Sing, SomeSing (..), fromSing,
                                             singByProxy, toSing)
import           System.Exit                (exitSuccess)
import           System.IO                  (hGetLine)
import           System.Log.Formatter       (simpleLogFormatter)
import           System.Log.Handler         (setFormatter)
import           System.Log.Handler.Simple  (fileHandler)
import qualified System.Log.Logger          as L
import           System.Process             (CreateProcess (..), ProcessHandle,
                                             StdStream (..), createProcess,
                                             proc)

import           Neovim                     hiding (Plugin)
import           Neovim.Context.Internal
import           Neovim.LSP.Protocol.Type


-------------------------------------------------------------------------------
-- Env
-------------------------------------------------------------------------------

type NeovimLsp = Neovim LspEnv

data ServerHandles = ServerHandles
  { serverIn  :: !Handle
  , serverOut :: !Handle
  , serverErr :: !Handle
  , serverPH  :: !ProcessHandle
  }

data Context = Context
  { _lspIdMap         :: !(HashMap ID ClientRequestMethod)
  , _lspUniqueID      :: !Int
  , _lspUniqueVersion :: !Version
  , _lspVersionMap    :: !(Map Uri Version)
  }

initialContext :: Context
initialContext = Context
  { _lspIdMap         = HM.empty
  , _lspUniqueID      = 0
  , _lspUniqueVersion = 0
  , _lspVersionMap    = M.empty
  }

-- TODO SenderやPluginとかのHandleを追加する
data LspEnv = LspEnv
  { _lspEnvServerHandles :: !(TVar (Maybe ServerHandles))
  ,  lspEnvInChan        :: !(TChan B.ByteString)
  , _lspEnvOutChan       :: !(TChan B.ByteString)
  , _lspEnvOpenedFiles   :: !(TVar (Map Uri Version))
  , _lspEnvContext       :: !(TVar Context)
  }
data PluginEnv = PluginEnv
  { _pluginEnvInChan  :: !(TChan InMessage)
  , _pluginEnvOutChan :: !(TChan ByteString)
  , _pluginEnvContext :: !(TVar Context)
  }

initialEnvM :: MonadIO m => m LspEnv
initialEnvM = liftIO $
  pure LspEnv
  <*> newTVarIO Nothing
  <*> newTChanIO
  <*> newTChanIO
  <*> newTVarIO M.empty
  <*> newTVarIO initialContext

-------------------------------------------------------------------------------
-- Plugin
-------------------------------------------------------------------------------

type PluginAction = Neovim PluginEnv

data Plugin = Plugin
  { pred  :: !(InMessage -> Bool) -- Maybe (TVar (InMessage -> Bool))にするか
  , acion :: !(PluginAction ())
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
methodOf (SomeNoti noti) =  InNoti $ fromSing $ singByProxy noti
methodOf (SomeReq  req ) =  InReq  $ fromSing $ singByProxy req
methodOf (SomeResp resp) =  InResp $ fromSing $ singByProxy resp

toInMessage :: HashMap ID ClientRequestMethod -> J.Value -> Either String InMessage
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
      | isJust (HM.lookup "id" o)
      , Just m <- fromJSONMay =<< HM.lookup "method" o
          = return $ InReq m
      | Just m <- fromJSONMay =<< HM.lookup "method" o
          = return $ InNoti m
      | Just m <- (`HM.lookup` map') =<< fromJSONMay =<< HM.lookup "id" o
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

type HasInChan'        env = HasInChan        env (TChan InMessage)
type HasOutChan'       env = HasOutChan       env (TChan B.ByteString)
type HasContext'       env = HasContext       env (TVar  Context)
type HasOpenedFiles'   env = HasOpenedFiles   env (TVar  (Map Uri Version))
type HasServerHandles' env = HasServerHandles env (TVar  (Maybe ServerHandles))

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

(%==) :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> (a -> a) -> m ()
(%==) = modifyOverTV

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
  inCh  <- asks lspEnvInChan
  outCh <- asks _lspEnvOutChan
  liftIO $ do
    hSetBuffering hin  NoBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering
    void $ async $ sender hin outCh
    void $ async $ receiver hout inCh
    _ <- async $ forever $ logger herr
    setupLogger
  where
    logger herr = handleAny
      (\e -> errorM $ "STDERR: Exception: " ++ show e)
      (hGetLine herr >>= (errorM . ("STDERR: "++)))

isInitialized :: NeovimLsp Bool
isInitialized = usesTV serverHandles isJust

uninitializedError :: a
uninitializedError = error "not initialized (TODO: fabulous message)"

-------------------------------------------------------------------------------
-- Finalize
-------------------------------------------------------------------------------

-- TODO
-- removeAllHandlersとか
-- serverのShutdownとか

-------------------------------------------------------------------------------
-- Logger
-------------------------------------------------------------------------------

setupLogger :: IO ()
setupLogger = do
  -- stderrには書き込みたくないときuncomment
  L.updateGlobalLogger L.rootLoggerName L.removeHandler
  L.updateGlobalLogger "nvim-hs-lsp" (L.setLevel L.DEBUG)
  h <- do lh <- fileHandler "/tmp/nvim-hs-lsp.log" L.DEBUG
          return $ setFormatter
                      lh
                      (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  L.updateGlobalLogger "nvim-hs-lsp" (L.addHandler h)
  L.infoM "nvim-hs-lsp" $ unlines
      [ ""
      , "＿人人人人人人＿"
      , "＞　__init__　＜"
      , "￣Y^Y^Y^Y^Y^Y^Y￣"
      ]

debugM :: MonadIO m => String -> m ()
debugM = liftIO . L.debugM "nvim-hs-lsp"

warningM :: MonadIO m => String -> m ()
warningM = liftIO . L.warningM "nvim-hs-lsp"

errorM :: MonadIO m => String -> m ()
errorM = liftIO . L.errorM "nvim-hs-lsp"

infoM :: MonadIO m => String -> m ()
infoM = liftIO . L.infoM "nvim-hs-lsp"

-------------------------------------------------------------------------------
-- Communication with Server
-------------------------------------------------------------------------------

sender :: Handle -> TChan B.ByteString -> IO ()
sender serverIn chan = do
  hSetBuffering serverIn NoBuffering
  forever $ do
    bs <- atomically $ readTChan chan
    B.hPutStr serverIn $ B.concat
        [ "Content-Length: "
        , B.pack $ show $ B.length bs
        , "\r\n\r\n"
        , bs
        ]
    debugM $ "=> " ++ B.unpack bs

receiver :: Handle -> TChan B.ByteString -> IO ()
receiver serverOut chan = forever $ do
    v <- receive serverOut
    atomically $ writeTChan chan v
  where
    receive :: Handle -> IO B.ByteString
    receive h = do
      len <- contentLength <$> readHeader h
      content <- B.hGet h len
      debugM $ "<= " ++ B.unpack content
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


-- Plugin's action
---------------------------------------

pull :: (HasInChan' env) => Neovim env InMessage
pull = liftIO . atomically . readTChan =<< view inChan

push :: (HasOutChan' env, J.ToJSON a, Show a) => a -> Neovim env ()
push x = do
  outCh <- view outChan
  liftIO $ atomically $ writeTChan outCh $ J.encode x

-- modifyMVarと一致させるなら関数を一つにまとめたほうがよい？
modifyContext :: (MonadReader env m, MonadIO m, HasContext' env)
              => (Context -> Context) -> (Context -> a) -> m a
modifyContext modify_ read_ = do
  ctxV <- view context
  ctx <- liftIO $ atomically $ do
    ctx <- readTVar ctxV
    writeTVar ctxV $! modify_ ctx
    return ctx
  return $ read_ ctx

---

modifyContext' :: (MonadReader env m, MonadIO m, HasContext' env)
               => (Context -> Context) -> m ()
modifyContext' modify_ = modifyContext modify_ (const ())

readContext :: (MonadReader env m, MonadIO m, HasContext' env)
            => (Context -> a) -> m a
readContext read_ = modifyContext id read_

---

genUniqueID :: (MonadReader env m, MonadIO m, HasContext' env) => m ID
genUniqueID = modifyContext
  (lspUniqueID %~ succ)
  (IDNum . fromIntegral . (^. lspUniqueID))

genUniqueVersion :: (MonadReader env m, MonadIO m, HasContext' env)
                 => m Version
genUniqueVersion = modifyContext
  (lspUniqueVersion %~ succ)
  (^. lspUniqueVersion)

addIdMethodMap :: (MonadReader env m, MonadIO m, HasContext' env)
               => ID -> ClientRequestMethod -> m ()
addIdMethodMap id' m = modifyContext' (lspIdMap %~ HM.insert id' m)

-------------------------------------------------------------------------------
-- Dispatcher
-------------------------------------------------------------------------------

-- TODO
asyncNeovim :: NFData a => iEnv -> Neovim iEnv a -> Neovim env (Async ())
asyncNeovim r a = do
    cfg <- ask'
    let threadConfig = retypeConfig r cfg
    liftIO . async . void $ runNeovim threadConfig a

dispatcher :: [Plugin] -> NeovimLsp (Async (), [Async ()])
dispatcher hs = do
    debugM "this is dispatcher!"
    LspEnv {  lspEnvInChan  = inChG
           , _lspEnvOutChan = outCh
           , _lspEnvContext = ctx
           } <- ask

    (handlers,hIDs) <- fmap unzip $ forM hs $ \(Plugin p action) -> do
      inCh <- liftIO newTChanIO
      let config = PluginEnv inCh outCh ctx
      hID <- asyncNeovim config $ loggingError action
      return ((p,inCh), hID)

    -- TODO handlersを動的に追加できるようにする？
    dpID <- liftIO $ async $ loggingError $ forever $ do
        rawInput <- atomically (readTChan inChG)
        let Just !v = J.decode rawInput
        idMap <- atomically (_lspIdMap <$> readTVar ctx)
        case toInMessage idMap v of
          Right !msg -> forM_ handlers $ \(p,inCh) ->
              when (p msg) $ atomically $ writeTChan inCh msg
          Left e -> errorM $ unlines
              [ "dispatcher: could not parse input."
              , "input: " ++ B.unpack rawInput
              , "error: " ++ show e
              ]

    return (dpID, hIDs)

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

loggingError :: (MonadUnliftIO m) => m a -> m a
loggingError = handleAny $ \e -> errorM (show e) >> throwIO e

