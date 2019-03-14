
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Neovim.LSP.Base
  ( NeovimLsp
  , LspEnv(..)
  , initialEnvM
  , LanguageEnv(..)
  , focusLang
  , Worker(..)
  , WorkerEnv(..)
  , WorkerAction
  , Context(..)
  , initialContext
  , ServerHandles(..)
  , OtherHandles(..)
  , LspConfig(..)
  , defaultLspConfig
  , Language
  , InMessage(..)

  -- Class
  , HasContext(..)
  , modifyContext
  , readContext
  , genUniqueID
  , genUniqueVersion
  , addIdMethodMap
  , HasInChan(..)
  , receiveMessage
  , HasOutChan(..)
  , sendRequest
  , sendRequest'
  , sendNotification
  , sendResponse

  , Callback(..)
  , CallbackOf
  , CallbackTicket(..)
  , registerCallback
  , getCallbackById
  , removeCallback
  , waitCallback
  , waitCallbackWithTimeout
  , nopCallback
  , withResponse

  , dispatch
  , registerAsyncHandle
  )
  where

import           RIO                          hiding ((^.))
import qualified RIO.HashMap                  as HM
import           RIO.List.Partial             (init)
import qualified RIO.Map                      as M
import qualified RIO.Text                     as T

import           Control.Lens                 (views)
import           Control.Lens.Operators
import           Control.Monad                (forM, forM_, forever)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (withReaderT)
import           Control.Monad.Trans.Resource (transResourceT)
import qualified Data.Aeson                   as J
import           Data.Constraint              (withDict)
import           Data.Generics.Product        (HasField' (..), field)
import           Data.Singletons              (Sing, SomeSing (..), fromSing,
                                               sing, singByProxy, toSing)
import           System.IO                    (openFile)
import           System.Process               (ProcessHandle)

import           LSP
import           Util
import           Neovim                       hiding (Plugin, (<>))
import qualified Neovim.Context.Internal      as Internal

-------------------------------------------------------------------------------
-- Env
-------------------------------------------------------------------------------

type NeovimLsp = Neovim LspEnv

-- | Enviroment of the main thread.
data LspEnv = LspEnv
  { logFunc          :: LogFunc
  , logFileHandle    :: Handle
  , logFuncFinalizer :: IO ()
  , languageMap      :: TVar (Map Language LanguageEnv)
  } deriving (Generic)

initialEnvM :: MonadIO m => FilePath -> m LspEnv
initialEnvM logFile = do
    logFileHandle <- liftIO $ openFile logFile AppendMode
    (logFunc, logFuncFinalizer) <- liftIO $ makeLogFunc logFileHandle
    languageMap <- newTVarIO M.empty
    return LspEnv {..}
  where
    makeLogFunc h = do
        hSetBuffering h LineBuffering
        opts <- setLogTerminal False .
                setLogUseColor False <$>
                logOptionsHandle h True
        newLogFunc opts

data LanguageEnv = LanguageEnv
   { logFunc       :: LogFunc
   , language      :: Language
   , serverHandles :: ServerHandles
   , otherHandles  :: TVar OtherHandles
   , inChan        :: TChan ByteString
   , outChan       :: TChan ByteString
   , context       :: TVar Context
   } deriving (Generic)

focusLang :: Language -> Neovim LanguageEnv a -> Neovim LspEnv (Maybe a)
focusLang lang m = usesTV (field @"languageMap") (M.lookup lang) >>= \case
    Nothing -> return Nothing
    Just x -> Just <$> retypeEnvNeovim (const x) m

-- | Enviroment of each plugin.
data WorkerEnv = WorkerEnv
   { inChan  :: TChan InMessage
   , outChan :: TChan ByteString
   , context :: TVar Context
   , logFunc :: LogFunc
   } deriving (Generic)

type WorkerAction = Neovim WorkerEnv

data Worker = Worker
  { pluginName   :: String
  , pluginAction :: WorkerAction ()
  }

-- | Context shared with main thread and all plugins.
data Context = Context
   { idMethodMap    :: Map ID ClientRequestMethod
   , uniqueID       :: Int
   , uniqueVersion  :: Version
   , versionMap     :: Map Uri Version
   , callbacks      :: Map ID Callback
   , lspConfig      :: LspConfig
   , diagnosticsMap :: Map Uri [Diagnostic]
   , openedFiles    :: Map Uri Version
   } deriving (Generic)

data Callback where
  Callback :: Typeable m => TMVar a -> CallbackOf m a -> Callback
type CallbackOf m a = ServerResponse m -> WorkerAction a
data CallbackTicket a = CallbackTicket
  { callbackID  :: ID
  , callbackVar :: TMVar a
  }

initialContext :: Context
initialContext = Context
  { idMethodMap    = M.empty
  , uniqueID       = 0
  , uniqueVersion  = 0
  , versionMap     = M.empty
  , callbacks      = M.empty
  , lspConfig      = defaultLspConfig
  , diagnosticsMap = M.empty
  , openedFiles    = M.empty
  }

data ServerHandles = ServerHandles
  { serverIn         :: Handle
  , serverOut        :: Handle
  , serverErr        :: Handle
  , serverProcHandle :: ProcessHandle
  } deriving (Generic)

-- sender, receiver, watcher of serverErr, dispatcher, plugins
newtype OtherHandles = OtherHandles
  { unOtherHandles :: [(String, Async ())] }
  -- 'String' refers to the name of Handle

data LspConfig = LspConfig
  { autoLoadQuickfix  :: Bool
  , settingsPath      :: Maybe FilePath
  , serverCommand     :: ~[String]
  , formattingOptions :: FormattingOptions
  } deriving (Generic, Show)

defaultLspConfig :: LspConfig
defaultLspConfig =  LspConfig
  { autoLoadQuickfix  = False
  , settingsPath      = Nothing
  , serverCommand     = error "No server command provided"
  , formattingOptions = FormattingOptions . Record
                      $ #tabSize @= 2
                     <! #insertSpaces @= True
                     <! nil
  }

type Language = Text

--instance NvimObject

-------------------------------------------------------------------------------
-- Worker
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Lens
-------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} HasField' "logFunc" env LogFunc
    => HasLogFunc env
  where
    logFuncL = field' @"logFunc"

class HasContext env where
  contextL :: Lens' env (TVar Context)
instance {-# OVERLAPPABLE #-} HasField' "context" env (TVar Context)
    => HasContext env
  where
    contextL = field' @"context"

class HasInChan env where
  inChanL :: Lens' env (TChan InMessage)
instance {-# OVERLAPPABLE #-} HasField' "inChan" env (TChan InMessage)
    => HasInChan env
  where
    inChanL = field' @"inChan"

class HasOutChan env where
  outChanL :: Lens' env (TChan ByteString)
instance {-# OVERLAPPABLE #-} HasField' "outChan" env (TChan ByteString)
    => HasOutChan env
  where
    outChanL = field' @"outChan"


-------------------------------------------------------------------------------
-- Access Context
-------------------------------------------------------------------------------

receiveMessage :: (HasInChan env, MonadReader env m, MonadIO m) => m InMessage
receiveMessage = liftIO . atomically . readTChan =<< view inChanL

sendMessage :: (HasOutChan env, J.ToJSON a, Show a, MonadReader env m, MonadIO m)
            => a -> m ()
sendMessage x = do
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
    field @"uniqueID" %~ (+1) &&&
    views (field @"uniqueID") (IDNum . fromIntegral)

genUniqueVersion :: (MonadReader env m, MonadIO m, HasContext env)
                 => m Version
genUniqueVersion = modifyReadContext $
    field @"uniqueVersion" %~ (+1) &&&
    view (field @"uniqueVersion")

addIdMethodMap :: (MonadReader env m, MonadIO m, HasContext env)
               => ID -> ClientRequestMethod -> m ()
addIdMethodMap id' m = modifyContext (field @"idMethodMap" %~ M.insert id' m)

registerCallback :: (MonadReader env m, MonadIO m, HasContext env)
                 => ID -> Callback -> m ()
registerCallback id' callback = modifyContext (field @"callbacks" %~ M.insert id' callback)

getCallbackById :: (MonadReader env m, MonadIO m, HasContext env)
                => ID -> m (Maybe Callback)
getCallbackById id' = readContext $ views (field @"callbacks") (M.lookup id')

removeCallback :: (MonadReader env m, MonadIO m, HasContext env)
               => ID -> m ()
removeCallback id' = modifyContext $ field @"callbacks" %~ M.delete id'

waitCallback :: (MonadIO m, MonadReader env m, HasLogFunc env)
             => m (CallbackTicket a) -> m a
waitCallback m = do
    x <- m
    logError $ "Waiting for callback: " <> displayShow (callbackID x)
    atomically $ takeTMVar $ callbackVar x

waitCallbackWithTimeout
  :: (MonadUnliftIO m, MonadReader env m, HasContext env, HasLogFunc env)
  => Int -> m (CallbackTicket a) -> m (Maybe a)
waitCallbackWithTimeout musec m = do
    c <- m
    timeout musec (waitCallback (return c)) >>= \case
      Nothing -> removeCallback (callbackID c) >> return Nothing
      Just x  -> return (Just x)

nopCallback :: ImplResponse m => CallbackOf m ()
nopCallback (Response resp) = void $ withResponse resp (const (return ()))

withResponse :: (HasLogFunc env, Show e)
             => ResponseMessage a e
             -> (a -> Neovim env ret)
             -> Neovim env (Maybe ret)
withResponse resp k =
  case resp^. #error of
    Some e -> vim_report_error' msg >> return Nothing
      where msg = "nvim-hs-lsp: error from server:"  <> T.unpack (prettyResponceError e)
    None -> case resp^. #result of
      None   -> logError "withCallbackResult: wrong input" >> return Nothing
      Some x -> Just <$> k x

-- | Because @m@ is not uniquely determined by type @RequestParam 'Client m@,
--   type annotation is always required when you use this function.
--   The GHC extension @-XTypeApplication@ is useful to do this.
--
-- > pushRequest @'InitializeK (initializeParam Nothing Nothing)
--
-- >>> :set -XTypeApplications -XDataKinds
-- >>> let wellTyped _ = "OK"
-- >>> wellTyped $ sendRequest @'InitializeK @LanguageEnv (initializeParam Nothing Nothing)
-- "OK"
--
sendRequest :: forall (m :: ClientRequestMethodK) env a
            .  (ImplRequest m, HasOutChan env, HasContext env)
            => RequestParam m
            -> CallbackOf m a
            -> Neovim env (CallbackTicket a)
sendRequest param callback = do
    let method = fromSing (sing :: Sing m)
    id' <- genUniqueID
    addIdMethodMap id' method
    sendMessage $ request @m id' param
    var <- newEmptyTMVarIO
    registerCallback id' $ Callback var callback
    return (CallbackTicket id' var)

sendRequest' :: forall (m :: ClientRequestMethodK) env
             .  (ImplRequest m, ImplResponse m, HasOutChan env, HasContext env)
             => RequestParam m
             -> Neovim env ()
sendRequest' param = void $ sendRequest @m param nopCallback

sendNotification :: forall (m :: ClientNotificationMethodK) env
                 .  (ImplNotification m, HasOutChan env)
                 => NotificationParam m -> Neovim env ()
sendNotification param = sendMessage $ notification @m param

sendResponse
  :: forall (m :: ServerRequestMethodK) env
  .  (ImplResponse m, HasOutChan env, HasContext env)
  => Nullable ID
  -> Option (ResponseResultParam m)
  -> Option (ResponseError (ResponseErrorParam m))
  -> Neovim env ()
sendResponse id' resp err' = sendMessage $ response @m id' resp err'

-------------------------------------------------------------------------------
-- Dispatcher
-------------------------------------------------------------------------------

dispatch :: [Worker] -> Neovim LanguageEnv ()
dispatch hs = do
    inChG   <- view $ field @"inChan"
    outCh   <- view $ field @"outChan"
    ctx     <- view $ field @"context"
    logFunc <- view $ field @"logFunc"

    inChs <- forM hs $ \(Worker name action) -> do
      inCh <- liftIO newTChanIO
      let workerEnv = WorkerEnv
                    { inChan  = inCh
                    , outChan = outCh
                    , context = ctx
                    , logFunc = logFunc
                    }
      a <- asyncNeovim workerEnv $ loggingErrorImmortal action
      registerAsyncHandle name a
      return inCh

    dispatcher <- async $ forever $ loggingErrorImmortal $ do
      rawInput <- atomically (readTChan inChG)
      let Just !v = J.decode (fromStrictBytes rawInput)
      idMethodMap <- view (field @"idMethodMap") <$> readTVarIO ctx
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
    field @"otherHandles" %== (\(OtherHandles hs) -> OtherHandles ((name,a):hs))

-------------------------------------------------------------------------------
-- Util for this module
-------------------------------------------------------------------------------

asyncNeovim :: NFData a => iEnv -> Neovim iEnv a -> Neovim env (Async a)
asyncNeovim r a = async $ retypeEnvNeovim (const r) a

retypeEnvNeovim :: (env -> env') -> Neovim env' a -> Neovim env a
retypeEnvNeovim f (Internal.Neovim m) =
  Internal.Neovim $
    transResourceT
      (withReaderT $ \cfg -> Internal.retypeConfig (f (Internal.customConfig cfg)) cfg)
      m

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

