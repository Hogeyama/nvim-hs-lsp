
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
{-# OPTIONS_GHC -Wall            #-}


module Neovim.LSP.Base where

import           Control.Concurrent         (ThreadId, forkIO)
import           Control.Concurrent.STM     (TVar, atomically, readTVar, writeTVar,
                                             TChan, newTChanIO, readTChan,
                                             writeTChan)
import           Control.Concurrent.Async   (async)
import           Control.Monad              (forM, forM_, forever, when)
import           Control.Monad.IO.Class     (liftIO, MonadIO)
import           Data.Bifunctor             (bimap)
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
                                             toSing, singByProxy)
import           System.Exit                (exitSuccess)
import           System.IO                  (BufferMode (..), Handle, hGetLine,
                                             hSetBuffering)
import           System.Process             (CreateProcess (..), ProcessHandle,
                                             StdStream (..), createProcess,
                                             proc)

import           Control.Lens               (makeLenses, uses)
import           Control.Lens.Operators
import qualified Data.Aeson                 as J
import           System.Log.Formatter       (simpleLogFormatter)
import           System.Log.Handler         (setFormatter)
import           System.Log.Handler.Simple  (fileHandler)
import qualified System.Log.Logger          as L

import           Neovim
import           Neovim.Context             (forkNeovim)
import           Neovim.LSP.Protocol.Type
import           Control.Monad.Catch        (catch, Exception, MonadCatch)

data Handles = Handles
  { _hin  :: !Handle
  , _hout :: !Handle
  , _herr :: !Handle
  , _ph   :: !ProcessHandle
  }
makeLenses ''Handles

-- TODO 全然足りないはず
data LSPState = LSPState
  { _server     :: !(Maybe Handles)
  , _outChannel :: TChan B.ByteString -- lazy initialization
  , _inChannel  :: TChan B.ByteString -- lazy initialization
  }
makeLenses ''LSPState

initialState :: LSPState
initialState = LSPState
  { _server     = Nothing
  , _outChannel = e
  , _inChannel  = e
  }
  where e = error "nvim-lsp: not initialized"

-------------------------------------------------------------------------------
-- Initialize
-------------------------------------------------------------------------------

initializeLSP :: String -> [String] -> Neovim r LSPState ()
initializeLSP cmd args = do
  (Just _hin, Just _hout, Just _herr, _ph) <- liftIO $
        createProcess $ (proc cmd args)
          { cwd = Just "/home/hogeyama/.local/bin/"
          , std_in  = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
  liftIO $ do hSetBuffering _hin  NoBuffering
              hSetBuffering _hout NoBuffering
              hSetBuffering _herr NoBuffering

  inCh  <- liftIO newTChanIO
  outCh <- liftIO newTChanIO
  void $ liftIO $ async $ sender _hin outCh
  void $ liftIO $ async $ receiver _hout inCh

  server     .= Just Handles{..}
  inChannel  .= inCh
  outChannel .= outCh
  liftIO setupLogger

isInitialized :: Neovim r LSPState Bool
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
  --L.updateGlobalLogger L.rootLoggerName L.removeHandler
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
-- Handler
-------------------------------------------------------------------------------

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

data HandlerConfig = HandlerConfig
  { inChan  :: !(TChan InMessage)
  , outChan :: !(TChan ByteString)
  , context :: !(TVar Context)
  }

type HandlerAction = Neovim HandlerConfig ()

data Handler = Handler
  { pred  :: !(InMessage -> Bool) -- Maybe (TVar (InMessage -> Bool))にするか
  , acion :: !(HandlerAction ())
  }

data InMessage where
  SomeNot :: ImplNotification m => !(ServerNotification m) -> InMessage
  SomeReq :: ImplRequest      m => !(ServerRequest      m) -> InMessage
  SomeRes :: ImplResponse     m => !(ServerResponse     m) -> InMessage
deriving instance Show InMessage

instance J.ToJSON InMessage where -- FromJSONは難しいかな
  toJSON (SomeNot x) = J.toJSON x
  toJSON (SomeReq x) = J.toJSON x
  toJSON (SomeRes x) = J.toJSON x

-- TODO 上手くやる
methodOf :: InMessage -> Either ClientMethod ServerMethod
methodOf (SomeNot noti) = Right . SNoti $ fromSing $ singByProxy noti
methodOf (SomeReq req ) = Right . SReq  $ fromSing $ singByProxy req
methodOf (SomeRes res ) = Left  . CReq  $ fromSing $ singByProxy res

toInMessage :: HashMap ID ClientRequestMethod -> J.Value -> Either String InMessage
toInMessage map' v@(J.Object o) = bimap toSing toSing <$> mmethod >>= \case
    Right (SomeSing (SSReq (s :: Sing m))) ->
        withDict (prfServerReq s) $ SomeReq @m <$> fromJSONEither v
    Right (SomeSing (SSNoti (s :: Sing m))) ->
        withDict (prfServerNoti s) $ SomeNot @m <$> fromJSONEither v
    Left (SomeSing (s :: Sing m)) ->
        withDict (prfServerRes s) $ SomeRes @m <$> fromJSONEither v
  where
    mmethod :: Either String (Either ClientRequestMethod ServerMethod)
    mmethod -- TODO error handling
      | Just m <- fromJSONMay =<< HM.lookup "method" o
          = return $ Right m
      | Just m <- (`HM.lookup` map') =<< fromJSONMay =<< HM.lookup "id" o
          = return $ Left m
      | otherwise
          = Left "hoge"
toInMessage _ _ = error "そんなバナナ"

-- Handler's action
---------------------------------------

-- TODO modifyTVarとか使ったほうが良い

pull :: Neovim HandlerConfig st InMessage
pull = liftIO . atomically . readTChan =<< asks inChan

-- (PrimMonad m, MonadReader HandlerConfig m)で十分ではある
push :: (J.ToJSON a, Show a) => a -> Neovim HandlerConfig st ()
push x = do
  outCh <- asks outChan
  liftIO $ atomically $ writeTChan outCh $ J.encode x

-- 修正前のをreadする
accessContext :: (Context -> Context) -> (Context -> a) -> Neovim HandlerConfig st a
accessContext modify_ read_ = do
  ctxV <- asks context
  ctx <- liftIO $ atomically $ do
    ctx <- readTVar ctxV
    writeTVar ctxV (modify_ ctx)
    return ctx
  return $ read_ ctx

---

modifyContext :: (Context -> Context) -> Neovim HandlerConfig st ()
modifyContext modify_ = accessContext modify_ (const ())

readContext :: (Context -> a) -> Neovim HandlerConfig st a
readContext read_ = accessContext id read_

---

genUniqueID :: Neovim HandlerConfig st ID
genUniqueID = accessContext
  (lspUniqueID %~ succ)
  (IDNum . fromIntegral . (^. lspUniqueID))

genUniqueVersion :: Neovim HandlerConfig st Version
genUniqueVersion = accessContext
  (lspUniqueVersion %~ succ)
  (^. lspUniqueVersion)

addIdMethodMap :: ID -> ClientRequestMethod -> Neovim HandlerConfig st ()
addIdMethodMap id' m = modifyContext (lspIdMap %~ HM.insert id' m)

-------------------------------------------------------------------------------
-- Dispatcher
-------------------------------------------------------------------------------

dispatcher :: TVar Context
           -> [Handler]
           -> Neovim r LSPState (ThreadId, [ThreadId])
dispatcher ctx hs = do
    debugM "this is dispatcher"
    lspState <- get
    let inChG = lspState ^. inChannel
        outCh = lspState ^. outChannel

    (handlers,hIDs) <- fmap unzip $ forM hs $ \(Handler p action) -> do
      inCh <- liftIO newTChanIO
      let config = HandlerConfig inCh outCh ctx
      hID <- forkNeovim config () $ withCatchBlah action
      return ((p,inCh), hID)

    -- run dispatcher
    dpID <- liftIO $ forkIO $ withCatchBlah $ forever $ do
        Just v <- J.decode <$> atomically (readTChan inChG)
        idMap  <- atomically (_lspIdMap <$> readTVar ctx)
        case toInMessage idMap v of
          Right msg -> forM_ handlers $ \(p,inCh) ->
              when (p msg) $ atomically $ writeTChan inCh msg
          Left e -> putStrLn e -- TODO これでいいのか

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

