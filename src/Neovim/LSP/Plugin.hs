
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.Plugin where

import           RIO
import           RIO.Char                          (isAlphaNum)
import qualified RIO.ByteString           as B
import           RIO.List                          (isPrefixOf, partition)
import qualified RIO.Map                           as M
import           RIO.Partial                       (read)

import           Control.Lens                      (views)
import           Control.Lens.Operators
import           Control.Monad.Extra               (ifM, whenJust, whenJustM)
import           Data.Aeson                        hiding (Object)
import qualified Data.ByteString.Char8             as BC
import           Data.Extensible
import           Data.Maybe                        (isJust)
import           System.Exit                       (exitSuccess)
import           System.IO                         (hGetLine)
import           System.IO.Error                   (isEOFError)
import           System.Process                    (CreateProcess (..),
                                                    StdStream (..),
                                                    createProcess, proc,
                                                    terminateProcess,
                                                    waitForProcess)

import           Neovim                            hiding (unlessM, whenM, (<>))
import           Neovim.LSP.Action.Notification
import           Neovim.LSP.Action.Request
import           Neovim.LSP.Base
import           Neovim.LSP.LspPlugin.Callback
import           Neovim.LSP.LspPlugin.Notification (notificationHandler)
import           Neovim.LSP.LspPlugin.Request      (requestHandler)
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

-------------------------------------------------------------------------------
-- Start Server
-------------------------------------------------------------------------------

startServer :: FilePath -> String -> [String] -> NeovimLsp ()
startServer cwd cmd args = do
    -- spawn
    --------
    (Just hin, Just hout, Just herr, ph) <-
        liftIO $ createProcess $ (proc cmd args)
          { cwd = Just cwd
          , std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
    #serverHandles .== Just ServerHandles
          { serverIn = hin
          , serverOut = hout
          , serverErr = herr
          , serverProcHandle = ph
          }
    liftIO $ do
      hSetBuffering hin  NoBuffering
      hSetBuffering hout NoBuffering
      hSetBuffering herr NoBuffering

    -- create connection
    --------------------
    inCh <- view #inChan
    outCh <- view #outChan
    registerAsyncHandle "sender" =<< async (sender hin outCh)
    registerAsyncHandle "receiver" =<< async (receiver hout inCh)
    registerAsyncHandle "herr-watcher" =<< async (watcher herr)

    -- settings
    -----------
    cfg <- loadLspConfig
    modifyContext $ #lspConfig .~ cfg
    dispatch [notificationHandler, requestHandler, callbackHandler]

    logInfo $ fromString $ unlines
        [ ""
        , "＿人人人人人人＿"
        , "＞　__init__　＜"
        , "￣Y^Y^Y^Y^Y^Y^Y￣"
        ]
    -- communication
    ----------------
    waitCallback $ pushRequest @'InitializeK
        (initializeParam Nothing (Just (filePathToUri cwd)))
        nopCallback
    pushNotification @'InitializedK (Record nil)
    whenJustM getWorkspaceSettings $ \v -> do
      let param = Record (#settings @= v <! nil)
      pushNotification @'WorkspaceDidChangeConfigurationK param
  where
    loadLspConfig :: HasContext env => Neovim env LspConfig
    loadLspConfig = updateLspConfig defaultLspConfig
      where
        updateLspConfig =
            update' "NvimHsLsp_autoLoadQuickfix" autoLoadQuickfix >=>
            update' "NvimHsLsp_settingsPath" settingsPath >=>
            update' "NvimHsLsp_serverCommands" lspCommands >=>
            return
          where
            update' var field before = vim_get_var var >>= \case
              Right o -> case fromObject o of
                Right new -> return $ before & field .~ new
                Left{} -> do
                  nvimEchoe $ "invalid config: " <> var
                  return before
              _ -> return before

    getWorkspaceSettings :: HasContext env => Neovim env (Maybe Value)
    getWorkspaceSettings = readContext (view (#lspConfig . settingsPath)) >>= \case
        Just file -> liftIO $ decodeFileStrict' file
        Nothing -> return Nothing

    watcher :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
            => Handle -> m ()
    watcher herr =
        do s <- B.hGetLine herr
           showAndLog $ "STDERR: " <> displayBytesUtf8 s
      `catchIO` \e ->
        if isEOFError e
        then throwIO e
        else showAndLog $ "STDERR: Exception: " <> displayShow e
      where
        showAndLog msg = do
            logError msg
            --vim_report_error' $ T.unpack $ utf8BuilderToText msg
            -- なんでRIOには'Utf8Builder -> String'の関数がないんだ
            -- rlsがうるさいのでreportやめます

    sender :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
           => Handle -> TChan ByteString -> m ()
    sender serverIn chan = forever $ loggingErrorDeep $ do
        bs <- atomically $ readTChan chan
        hPutBuilder serverIn $ getUtf8Builder $ mconcat
            [ "Content-Length: "
            , displayShow $ B.length bs
            , "\r\n\r\n"
            , displayBytesUtf8 bs
            ]
        logDebug $ "=> " <> displayBytesUtf8 bs


    receiver :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
             => Handle -> TChan ByteString -> m ()
    receiver serverOut chan = forever $ loggingErrorDeep $ do
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



isInitialized :: NeovimLsp Bool
isInitialized = usesTV #serverHandles isJust

uninitializedError :: a
uninitializedError = error "not initialized (TODO: fabulous message)"

-------------------------------------------------------------------------------
-- Initialize
-------------------------------------------------------------------------------

nvimHsLspInitialize :: CommandArguments -> NeovimLsp ()
nvimHsLspInitialize _ = loggingErrorImmortal $ do
  initialized <- isInitialized
  if initialized then do
    vim_out_write' $ "nvim-hs-lsp: Already initialized" ++ "\n"
  else do
    mft <- getBufLanguage =<< vim_get_current_buffer'
    case mft of
      Just ft -> do
        map' <- errOnInvalidResult $ vim_get_var "NvimHsLsp_serverCommands"
        case M.lookup ft map' of
          Just (cmd:args) -> do
            cwd <- errOnInvalidResult (vimCallFunction "getcwd" [])
            startServer cwd cmd args
            #fileType .== Just ft
            let pat = def { acmdPattern = "*" }
                arg = def { bang = Just True }
            Just Right{} <- addAutocmd "BufRead,BufNewFile"
                              pat (nvimHsLspOpenBuffer arg)
            Just Right{} <- addAutocmd "TextChanged,TextChangedI"
                              pat (nvimHsLspChangeBuffer arg)
            Just Right{} <- addAutocmd "BufWrite"
                              pat (nvimHsLspSaveBuffer arg)
            nvimHsLspOpenBuffer def
            whenM (readContext . view $ #lspConfig.autoLoadQuickfix) (vim_command' "copen")
            vim_out_write' $
              "nvim-hs-lsp: Initialized for filetype `" ++ ft ++ "`\n"
          _ ->
            vim_report_error' $
              "no language server registered for filetype `" ++ ft ++ "`"
      Nothing ->
        vim_report_error'
          "nvim-hs-lsp: Could not initialize: Could not determine the filetype"

whenInitialized' :: Bool -> NeovimLsp () -> NeovimLsp ()
whenInitialized' silent m = isInitialized >>= \case
  True  -> m
  False -> unless silent $
    vim_out_write' $ "nvim-hs-lsp: Not initialized!" ++ "\n"

whenInitialized :: NeovimLsp () -> NeovimLsp ()
whenInitialized = whenInitialized' False

-------------------------------------------------------------------------------
-- stop server
-------------------------------------------------------------------------------

stopServer :: NeovimLsp ()
stopServer = do
    push $ notification @'ExitK exitParam
    useTV #serverHandles >>= \case
      Nothing -> return ()
      Just sh -> do
        stillAlive <- fmap isNothing $ timeout (1 * 1000 * 1000) $
          liftIO $ waitForProcess (serverProcHandle sh)
        when stillAlive $
          liftIO $ terminateProcess (serverProcHandle sh)
    mapM_ (cancel.snd) =<< usesTV #otherHandles unOtherHandles
    #serverHandles .== Nothing
    #fileType .== Nothing

    -- TODO
    -- #context .== initialContext

-------------------------------------------------------------------------------
-- Notification
-------------------------------------------------------------------------------

whenAlreadyOpened' :: Bool -> NeovimLsp () -> NeovimLsp ()
whenAlreadyOpened' silent m = do
  uri <- getBufUri =<< vim_get_current_buffer'
  ifM (alreadyOpened uri) m (unless silent $ vim_out_write' "nvim-hs-lsp: Not opened yet\n")

whenAlreadyOpened :: NeovimLsp () -> NeovimLsp ()
whenAlreadyOpened = whenAlreadyOpened' False

alreadyOpened :: Uri -> NeovimLsp Bool
alreadyOpened uri = -- M.member uri <$> useTV #openedFiles
  readContext (views #openedFiles (M.member uri))

nvimHsLspOpenBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspOpenBuffer arg = whenInitialized' silent $ do
  b   <- vim_get_current_buffer'
  mft <- getBufLanguage b
  whenJust mft $ \ft -> do
    serverFT <- useTV #fileType
    when (Just ft == serverFT) $ do
      uri <- getBufUri b
      unlessM (alreadyOpened uri) $ do
        modifyContext $ #openedFiles %~ M.insert uri 0
        didOpenBuffer b
  where silent = Just True == bang arg

nvimHsLspCloseBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspCloseBuffer _ = whenInitialized $ do
  b <- vim_get_current_buffer'
  uri <- getBufUri b
  ifM (not <$> alreadyOpened uri) (vim_out_write' "nvim-hs-lsp: Not opened yet\n") $ do
    modifyContext $ #openedFiles %~ M.delete uri
    didCloseBuffer b

nvimHsLspChangeBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspChangeBuffer arg = whenInitialized' silent $ whenAlreadyOpened' silent $
  didChangeBuffer =<< vim_get_current_buffer'
  where silent = Just True == bang arg

nvimHsLspSaveBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspSaveBuffer arg = whenInitialized' silent $ whenAlreadyOpened' silent $
  didSaveBuffer =<< vim_get_current_buffer'
  where silent = Just True == bang arg

nvimHsLspExit :: CommandArguments -> NeovimLsp ()
nvimHsLspExit _ = whenInitialized $ do
  stopServer
  finalize

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

-- Hover
--------
nvimHsLspInfo :: CommandArguments -> NeovimLsp ()
nvimHsLspInfo _ = whenInitialized . whenAlreadyOpened . loggingError $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ hoverRequest b pos callbackHoverOneLine

nvimHsLspHover :: CommandArguments -> NeovimLsp ()
nvimHsLspHover _ = whenInitialized . whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ hoverRequest b pos callbackHoverPreview

-- Definition
-------------
nvimHsLspDefinition :: CommandArguments -> NeovimLsp ()
nvimHsLspDefinition _ = whenInitialized . whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ definitionRequest b pos callbackDefinition

-- Completion
-------------

-- Sync
nvimHsLspComplete :: Object -> String
                  -> NeovimLsp (Either Int [VimCompleteItem])
nvimHsLspComplete findstart base = do
  notInitialized <- not <$> isInitialized
  if notInitialized then
    return (Left (-1))
  else do
    let findStart = case findstart of
          ObjectInt n -> n
          ObjectString s
            | Just n <- readMaybe (BC.unpack s) -> n
          _ -> error "流石にここに来たら怒っていいよね"
    if findStart == 1 then do
      s   <- nvim_get_current_line'
      col <- snd <$> getNvimPos
      return $ Left $ completionFindStart s col
    else do
      curPos <- getNvimPos
      let compPos = completionPos base curPos
      b <- vim_get_current_buffer'
      xs <- waitCallback $ completionRequest b compPos callbackComplete
      let sorted = uncurry (++) $ partition (isPrefixOf base . view #word . fields)  xs
      return (Right sorted)

completionFindStart :: String -> Int -> Int
completionFindStart curLine col =
  let isKeyword c = isAlphaNum c || (c `elem` ['_','\''])
      foo = reverse $ dropWhile isKeyword $ reverse $ take (col-1) curLine
      len = length foo
  in  len

completionPos :: String -> NvimPos -> NvimPos
completionPos base (line, col) = (line, col+length base)

-- Async
nvimHsLspAsyncComplete :: Int -> Int -> NeovimLsp ()
nvimHsLspAsyncComplete lnum col = do
  initialized <- isInitialized
  if initialized then do
    b <- vim_get_current_buffer'
    s <- nvim_get_current_line'
    logDebug $ "COMPLETION: async: col = " <> displayShow col
    logDebug $ "COMPLETION: async: s   = " <> displayShow (take col s)
    xs <- waitCallback $ completionRequest b (lnum,col) callbackComplete
    nvim_set_var' nvimHsCompleteResultVar (toObject xs)
  else do
    nvim_set_var' nvimHsCompleteResultVar (toObject ([]::[VimCompleteItem]))

nvimHsCompleteResultVar :: String
nvimHsCompleteResultVar = "NvimHsLspCompleteResult"

-------------------------------------------------------------------------------
-- Load Quickfix (not a command)
-------------------------------------------------------------------------------

nvimHsLspLoadQuickfix :: CommandArguments -> NeovimLsp ()
nvimHsLspLoadQuickfix arg = do
    allDiagnostics <- readContext $ view #diagnosticsMap
    curi <- getBufUri =<< nvim_get_current_buf'
    let qfItems = if showAll
                  then diagnosticsToQfItems curi allDiagnostics
                  else diagnosticsToQfItems curi $
                          M.singleton curi
                          (M.findWithDefault [] curi allDiagnostics)
                  -- TODO
    replaceQfList qfItems
    if null qfItems
      then vim_command' "cclose" >> nvimEcho "nvim-hs-lsp: no diagnostics"
      else vim_command' "ccopen"
  where
    showAll = Just True == bang arg

-------------------------------------------------------------------------------
-- CodeAction
-------------------------------------------------------------------------------

nvimHsLspCodeAction :: CommandArguments -> NeovimLsp ()
nvimHsLspCodeAction _ = whenInitialized . whenAlreadyOpened $ do
    b <- vim_get_current_buffer'
    pos <- getNvimPos
    waitCallback $ codeAction b (pos,pos) callbackCodeAction

-- HIE specific
---------------

nvimHsLspHieCaseSplit :: CommandArguments -> NeovimLsp ()
nvimHsLspHieCaseSplit _ = hiePointCommand "ghcmod:casesplit"

hiePointCommand :: String -> NeovimLsp ()
hiePointCommand cmd =  whenInitialized . whenAlreadyOpened $ do
    uri <- getBufUri =<< vim_get_current_buffer'
    pos <- getNvimPos
    let arg = toJSON $ #file @= uri
                    <! #pos  @= nvimPosToPosition pos
                    <! nil @(Field Identity)
    void $ executeCommandRequest cmd (Some [arg]) Nothing

nvimHsLspHieHsImport :: CommandArguments -> String -> NeovimLsp ()
nvimHsLspHieHsImport _ moduleToImport = do
    uri <- getBufUri =<< vim_get_current_buffer'
    let arg = toJSON $ #file @= uri
                    <! #moduleToImport @= moduleToImport
                    <! nil @(Field Identity)
    void $ executeCommandRequest "hsimport:import" (Some [arg]) Nothing


-------------------------------------------------------------------------------
-- Formatting
-------------------------------------------------------------------------------

nvimHsLspFormatting :: CommandArguments -> NeovimLsp ()
nvimHsLspFormatting CommandArguments{range,bang} = whenInitialized . whenAlreadyOpened $ do
    b <- vim_get_current_buffer'
    let fopts = FormattingOptions . Record
              $ #tabSize @= 2 -- TODO set by vim variable
             <! #insertSpaces @= False
             <! nil
    case (bang, range) of
      (Just True, _) ->
        waitCallback $ textDocumentFormatting b fopts
      (_, Nothing) ->
        waitCallback $ textDocumentFormatting b fopts
      (_, Just (l1,l2)) ->
        waitCallback $ textDocumentRangeFormatting b ((l1,1),(l2,1)) fopts

-------------------------------------------------------------------------------
-- References
-------------------------------------------------------------------------------

nvimHsLspReferences :: CommandArguments -> NeovimLsp ()
nvimHsLspReferences _ = whenInitialized . whenAlreadyOpened $ do
    b <- vim_get_current_buffer'
    p <- getNvimPos
    waitCallback $ textDocumentReferences b p callbackTextDocumentReferences

-------------------------------------------------------------------------------
-- DocumentSymbol
-------------------------------------------------------------------------------

nvimHsLspDocumentSymbol :: CommandArguments -> NeovimLsp ()
nvimHsLspDocumentSymbol _ = whenInitialized . whenAlreadyOpened $ do
    b <- vim_get_current_buffer'
    waitCallback $ textDocumentDocumentSymbol b

-------------------------------------------------------------------------------
-- WorkspaceSymbol
-------------------------------------------------------------------------------

nvimHsLspWorkspaceSymbol :: CommandArguments -> String -> NeovimLsp ()
nvimHsLspWorkspaceSymbol _ sym = whenInitialized . whenAlreadyOpened $ do
    waitCallback $ workspaceSymbol sym callbackWorkspaceSymbol

