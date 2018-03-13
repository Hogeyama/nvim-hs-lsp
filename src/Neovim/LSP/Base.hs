
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wall            #-}


module Neovim.LSP.Base where

import           Control.Concurrent         (ThreadId, forkIO)
import           Control.Concurrent.Async   (async)
import           Control.Concurrent.STM     (TChan, TVar, atomically,
                                             newTChanIO, readTChan, readTVar,
                                             writeTChan, writeTVar)
import           Control.Monad              (forM, forM_, forever, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
--import           Data.Bifunctor             (bimap)
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
import           System.IO                  (BufferMode (..), Handle, hGetLine,
                                             hSetBuffering)
import           System.Process             (CreateProcess (..), ProcessHandle,
                                             StdStream (..), createProcess,
                                             proc)

import           Control.Lens               hiding (Context)--(makeLenses, uses)
import qualified Data.Aeson                 as J
import           System.Log.Formatter       (simpleLogFormatter)
import           System.Log.Handler         (setFormatter)
import           System.Log.Handler.Simple  (fileHandler)
import qualified System.Log.Logger          as L

import           Control.Monad.Catch        (Exception, MonadCatch, catch)
import           Neovim                     hiding (Plugin)
import           Neovim.Context             (forkNeovim)
import           Neovim.LSP.Protocol.Type
import           GHC.Conc.Sync (newTVarIO)

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
makeLenses ''Context

initialContext :: Context
initialContext = Context
  { _lspIdMap         = HM.empty
  , _lspUniqueID      = 0
  , _lspUniqueVersion = 0
  , _lspVersionMap    = M.empty
  }

-- TODO SenderやPluginとかのHandleを追加する
data LspState = LspState
  { _server     :: !(Maybe ServerHandles)
  }
makeLenses ''LspState

data LspEnv = LspEnv
  { serverInChan  :: TChan B.ByteString -- lazy initialization
  , serverOutChan :: TChan B.ByteString -- lazy initialization
  , contextG      :: !(TVar Context)
  }
makeLenses ''LspEnv

initialEnvIO :: IO LspEnv
initialEnvIO = LspEnv <$> newTChanIO <*> newTChanIO <*> newTVarIO initialContext

initialState :: LspState
initialState = LspState
  { _server     = Nothing
  }

type NeovimLsp = Neovim LspEnv LspState

class HasContext r where contextL :: Lens' r (TVar Context)
instance HasContext PluginEnv where
  contextL = lens context (\x y -> x { context = y})
instance HasContext LspEnv where
  contextL = lens contextG (\x y -> x { contextG = y})

class HasInChannel r where inChanL :: Lens' r (TChan InMessage)
instance HasInChannel PluginEnv where
  inChanL = lens inChan (\x y -> x { inChan = y})

class HasOutChannel r where outChanL :: Lens' r (TChan B.ByteString)
instance HasOutChannel PluginEnv where
  outChanL = lens outChan (\x y -> x { outChan = y})
instance HasOutChannel LspEnv where
  outChanL = lens serverOutChan (\x y -> x { serverOutChan = y})

-------------------------------------------------------------------------------
-- Initialize
-------------------------------------------------------------------------------

initializeLsp :: String -> [String] -> Neovim LspEnv LspState ()
initializeLsp cmd args = do
  (Just hin, Just hout, Just herr, ph) <-
      liftIO $ createProcess $ (proc cmd args)
        { cwd     = Nothing
        , std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  server .= Just (ServerHandles hin hout herr ph)
  inCh <- asks serverInChan
  outCh <- asks serverOutChan
  liftIO $ do
    hSetBuffering hin  NoBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering
    void $ async $ sender hin outCh
    void $ async $ receiver hout inCh
    setupLogger

isInitialized :: Neovim r LspState Bool
isInitialized = uses server isJust

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


-------------------------------------------------------------------------------
-- Plugin
-------------------------------------------------------------------------------

data PluginEnv = PluginEnv
  { inChan  :: !(TChan InMessage)
  , outChan :: !(TChan ByteString)
  , context :: !(TVar Context)
  }

type PluginAction = Neovim PluginEnv ()

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
  = IMReq  ServerRequestMethod
  | IMNoti ServerNotificationMethod
  | IMResp ClientRequestMethod

methodOf :: InMessage -> InMessageMethod
methodOf (SomeNoti noti) =  IMNoti $ fromSing $ singByProxy noti
methodOf (SomeReq  req ) =  IMReq  $ fromSing $ singByProxy req
methodOf (SomeResp resp) =  IMResp $ fromSing $ singByProxy resp

toInMessage :: HashMap ID ClientRequestMethod -> J.Value -> Either String InMessage
toInMessage map' v@(J.Object o) = mmethod >>= \case
    IMReq (toSing -> SomeSing (s :: Sing m))
      -> withDict (prfServerReq s) $ SomeReq  @m <$> fromJSONEither v
    IMNoti (toSing -> SomeSing (s :: Sing m))
      -> withDict (prfServerNoti s) $ SomeNoti @m <$> fromJSONEither v
    IMResp (toSing -> SomeSing (s :: Sing m))
      -> withDict (prfServerResp s) $ SomeResp @m <$> fromJSONEither v
  where
    -- TODO
    -- + error handling
    -- + Miscが来たときに必ずNotiになってしまう
    --   多分globalな状態で持ってどっちかを指定するのが良いのだと思う
    --   `cancel`はNotificationとか
    mmethod :: Either String InMessageMethod
    mmethod
      | Just m <- fromJSONMay =<< HM.lookup "method" o
          = return $ IMNoti m
      | Just m <- fromJSONMay =<< HM.lookup "method" o
          = return $ IMReq m
      | Just m <- (`HM.lookup` map') =<< fromJSONMay =<< HM.lookup "id" o
          = return $ IMResp m
      | otherwise
          = Left "そんなバナナ1"
toInMessage _ _ = error "そんなバナナ2"

-- Plugin's action
---------------------------------------

-- TODO
-- + modifyTVarとか使ったほうが良い
--    + STMはどう使えばsafeなんだろうな
--    + MVarよりも安全っぽい？ <= YES!

pull :: (HasInChannel r) => Neovim r st InMessage
pull = liftIO . atomically . readTChan =<< view inChanL

push :: (HasOutChannel r, J.ToJSON a, Show a) => a -> Neovim r st ()
push x = do
  outCh <- view outChanL
  liftIO $ atomically $ writeTChan outCh $ J.encode x

-- modifyMVarと一致させるなら関数を一つにまとめたほうがよい？
modifyContext :: HasContext r
              => (Context -> Context) -> (Context -> a) -> Neovim r st a
modifyContext modify_ read_ = do
  ctxV <- view contextL
  ctx <- liftIO $ atomically $ do
    ctx <- readTVar ctxV
    writeTVar ctxV $! modify_ ctx
    return ctx
  return $ read_ ctx

---

modifyContext' :: HasContext r => (Context -> Context) -> Neovim r st ()
modifyContext' modify_ = modifyContext modify_ (const ())

readContext :: HasContext r => (Context -> a) -> Neovim r st a
readContext read_ = modifyContext id read_

---

genUniqueID :: HasContext r => Neovim r st ID
genUniqueID = modifyContext
  (lspUniqueID %~ succ)
  (IDNum . fromIntegral . (^. lspUniqueID))

genUniqueVersion :: HasContext r => Neovim r st Version
genUniqueVersion = modifyContext
  (lspUniqueVersion %~ succ)
  (^. lspUniqueVersion)

addIdMethodMap :: HasContext r => ID -> ClientRequestMethod -> Neovim r st ()
addIdMethodMap id' m = modifyContext' (lspIdMap %~ HM.insert id' m)

-------------------------------------------------------------------------------
-- Dispatcher
-------------------------------------------------------------------------------

dispatcher :: [Plugin]
           -> Neovim LspEnv LspState (ThreadId, [ThreadId])
dispatcher hs = do
    debugM "this is dispatcher!"
    LspEnv inChG outCh ctx <- ask

    (handlers,hIDs) <- fmap unzip $ forM hs $ \(Plugin p action) -> do
      inCh <- liftIO newTChanIO
      let config = PluginEnv inCh outCh ctx
      hID <- forkNeovim config () $ withCatchBlah action
      return ((p,inCh), hID)

    -- TODO handlersを動的に追加できるように
    dpID <- liftIO $ forkIO $ withCatchBlah $ forever $ do
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
  where
    withCatchBlah :: (MonadIO m, MonadCatch m) => m () -> m ()
    withCatchBlah m = m `catch` \case Blah -> return ()

data Blah = Blah deriving Show
instance Exception Blah

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

fromJSONMay :: J.FromJSON a => J.Value -> Maybe a
fromJSONMay = resultMaybe . J.fromJSON

fromJSONEither :: J.FromJSON a => J.Value -> Either String a
fromJSONEither = resultEither . J.fromJSON

resultMaybe :: J.Result a -> Maybe a
resultMaybe (J.Success x) = Just x
resultMaybe _             = Nothing

resultEither :: J.Result a -> Either String a
resultEither (J.Success x) = Right x
resultEither (J.Error e)   = Left e

