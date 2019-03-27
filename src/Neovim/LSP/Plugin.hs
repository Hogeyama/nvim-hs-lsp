
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.Plugin where

import           RIO                               hiding ((^.))
import           RIO.Char                          (isAlphaNum)
import           RIO.List                          (isPrefixOf, partition)
import qualified RIO.Map                           as M

import           Control.Lens                      (views)
import           Control.Lens.Operators
import           Control.Monad.Extra               (ifM, whenJust)
import           Data.Aeson                        hiding (Object)
import qualified Data.ByteString.Char8             as BC
import           Data.Generics.Product             (field)

import           LSP
import           Util
import           Neovim                            hiding (unlessM, whenM, (<>))
import           Neovim.LSP.Action.Notification
import           Neovim.LSP.Action.Request
import           Neovim.LSP.Base
import           Neovim.LSP.LspPlugin.Callback
import           Neovim.LSP.LspPlugin.Notification (notificationHandler)
import           Neovim.LSP.LspPlugin.Request      (requestHandler)
import           Neovim.LSP.Util

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

isInitialized :: NeovimLsp Bool
isInitialized = usesTV (field @"languageMap") (not . null)

whenInitialized' :: Bool -> NeovimLsp () -> NeovimLsp ()
whenInitialized' silent m = isInitialized >>= \case
  True  -> m
  False -> unless silent $
    vim_out_write $ "nvim-hs-lsp: Not initialized!" ++ "\n"

whenInitialized :: NeovimLsp () -> NeovimLsp ()
whenInitialized = whenInitialized' False

focusLang' :: Bool -> Neovim LanguageEnv () -> NeovimLsp ()
focusLang' silent m = vim_get_current_buffer >>= getBufLanguage >>= \case
    Nothing -> error' "unknown filetype"
    Just lang ->
      focusLang (fromString lang) m >>= \case
        Nothing -> error' $ "language server for " ++ lang ++ " is not yet awaken"
        Just () -> return ()
  where
    error' = if silent then const (return ()) else error

whenAlreadyOpened' :: Bool -> Neovim LanguageEnv () -> Neovim LanguageEnv ()
whenAlreadyOpened' silent m = do
  uri <- getBufUri =<< vim_get_current_buffer
  ifM (alreadyOpened uri)
    m
    (unless silent $ vim_out_write "nvim-hs-lsp: Not opened yet\n")

whenAlreadyOpened :: Neovim LanguageEnv () -> Neovim LanguageEnv ()
whenAlreadyOpened = whenAlreadyOpened' False

alreadyOpened :: (MonadReader env m, MonadIO m, HasContext env) => Uri -> m Bool
alreadyOpened uri = readContext $ views (field @"openedFiles") (M.member uri)

-------------------------------------------------------------------------------
-- Initialize
-------------------------------------------------------------------------------

nvimHsLspInitialize :: CommandArguments -> NeovimLsp ()
nvimHsLspInitialize _ = loggingError $ do
    mft <- getBufLanguage =<< vim_get_current_buffer
    case mft of
      Just lang -> do
        languageMap <- useTV (field @"languageMap")
        if M.member (fromString lang) languageMap then
          vim_out_write $ "nvim-hs-lsp: Already initialized" ++ "\n"
        else do
          cwd <- getCwd
          startServer (fromString lang) cwd
            [ notificationHandler, requestHandler, callbackHandler ]
          let pat = def { acmdPattern = "*" }
              arg = def { bang = Just True }
          Just Right{} <- addAutocmd "BufRead,BufNewFile"
                            pat (nvimHsLspOpenBuffer arg)
          Just Right{} <- addAutocmd "TextChanged,TextChangedI"
                            pat (nvimHsLspChangeBuffer arg)
          Just Right{} <- addAutocmd "BufWrite"
                            pat (nvimHsLspSaveBuffer arg)
          nvimHsLspOpenBuffer def
          void $ focusLang (fromString lang) $
            whenM (readContext . view $
                    field @"lspConfig".
                    field @"autoLoadQuickfix")
                  (vimCommand "copen")
          vim_out_write $
            "nvim-hs-lsp: Initialized for filetype `" ++ lang ++ "`\n"
            -- _ ->
            --   vim_report_error' $
            --     "no language server registered for filetype `" ++ lang ++ "`"
      Nothing ->
        vim_report_error
          "nvim-hs-lsp: Could not initialize: Could not determine the filetype"

nvimHsLspStartServer :: CommandArguments -> NeovimLsp ()
nvimHsLspStartServer = nvimHsLspInitialize

-------------------------------------------------------------------------------
-- Notification
-------------------------------------------------------------------------------

nvimHsLspOpenBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspOpenBuffer _arg = focusLang' True $ do
  b   <- vim_get_current_buffer
  mft <- getBufLanguage b
  whenJust mft $ \ft -> do
    serverFT <- view (field @"language")
    when (fromString ft == serverFT) $ do
      uri <- getBufUri b
      unlessM (alreadyOpened uri) $ do
        modifyContext $ field @"openedFiles" %~ M.insert uri 0
        didOpenBuffer b

nvimHsLspCloseBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspCloseBuffer _ = focusLang' True $ do
  b <- vim_get_current_buffer
  uri <- getBufUri b
  ifM (not <$> alreadyOpened uri) (vim_out_write "nvim-hs-lsp: Not opened yet\n") $ do
    modifyContext $ field @"openedFiles" %~ M.delete uri
    didCloseBuffer b

nvimHsLspChangeBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspChangeBuffer arg = whenInitialized' silent $ focusLang' True $ whenAlreadyOpened' silent $
    didChangeBuffer =<< vim_get_current_buffer
  where silent = Just True == bang arg

nvimHsLspSaveBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspSaveBuffer arg = whenInitialized' silent $ focusLang' True $ whenAlreadyOpened' silent $
    didSaveBuffer =<< vim_get_current_buffer
  where silent = Just True == bang arg

nvimHsLspStopServer :: CommandArguments -> NeovimLsp ()
nvimHsLspStopServer _ = vim_get_current_buffer >>= getBufLanguage >>= \case
    Nothing -> return () -- TODO error
    Just lang -> stopServer (fromString lang)

-- Exitは別に作る
nvimHsLspExit :: CommandArguments -> NeovimLsp ()
nvimHsLspExit _ = vim_get_current_buffer >>= getBufLanguage >>= \case
    Nothing -> return () -- TODO error
    Just lang -> stopServer (fromString lang)

-- 関数とかを解放
-- TODO
nvimHsLspFinalize :: CommandArguments -> NeovimLsp ()
nvimHsLspFinalize _ = do
    undefined

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

-- Hover
--------
nvimHsLspInfo :: CommandArguments -> NeovimLsp ()
nvimHsLspInfo _ = whenInitialized $ focusLang' False $ whenAlreadyOpened . loggingError $ do
  uri <- getBufUri =<< vim_get_current_buffer
  pos <- fromNvimPos <$> getNvimPos
  void $ hoverRequest uri pos callbackHoverOneLine

nvimHsLspHover :: CommandArguments -> NeovimLsp ()
nvimHsLspHover _ = whenInitialized $ focusLang' False $ whenAlreadyOpened $ do
  uri <- getBufUri =<< vim_get_current_buffer
  pos <- fromNvimPos <$> getNvimPos
  void $ hoverRequest uri pos callbackHoverPreview

-- Definition
-------------
nvimHsLspDefinition :: CommandArguments -> NeovimLsp ()
nvimHsLspDefinition _ = whenInitialized $ focusLang' False $ whenAlreadyOpened $ do
  uri <- getBufUri =<< vim_get_current_buffer
  pos <- fromNvimPos <$> getNvimPos
  void $ definitionRequest uri pos callbackDefinition

-- Completion
-------------

-- Sync
nvimHsLspComplete :: Object -> String
                  -> NeovimLsp (Either Int [VimCompleteItem])
nvimHsLspComplete findstart base =
  vim_get_current_buffer >>= getBufLanguage >>= \case
    Nothing -> return (Left (-1))
    Just (fromString -> lang) -> fmap (fromMaybe (Left (-1))) $
      focusLang lang $ do
        let findStart = case findstart of
              ObjectInt n -> n
              ObjectString s
                | Just n <- readMaybe (BC.unpack s) -> n
              _ -> error "流石にここに来たら怒っていいよね"
        if findStart == 1 then do
          s   <- nvim_get_current_line
          col <- snd <$> getNvimPos
          return $ Left $ completionFindStart s col
        else do
          curPos <- getNvimPos
          let compPos = fromNvimPos $ completionPos base curPos
          uri <- getBufUri =<< vim_get_current_buffer
          xs <- fromMaybe [] <$>
                  waitCallbackWithTimeout (1*1000*1000)
                    (completionRequest uri compPos callbackComplete)
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
nvimHsLspAsyncComplete lnum col =
  vim_get_current_buffer >>= getBufLanguage >>= \case
    Just (fromString -> lang) -> check $ focusLang lang $ do
      let pos = fromNvimPos (lnum,col)
      uri <- getBufUri =<< vim_get_current_buffer
      xs <- fromMaybe [] <$>
              waitCallbackWithTimeout (1*1000*1000)
                (completionRequest uri pos callbackComplete)
      nvim_set_var nvimHsCompleteResultVar (toObject xs)
    Nothing ->
      recover
  where
    check m = m >>= \case
      Nothing -> recover
      Just () -> return ()
    recover = nvim_set_var nvimHsCompleteResultVar ObjectNil

nvimHsCompleteResultVar :: String
nvimHsCompleteResultVar = "NvimHsLspCompleteResult"

-------------------------------------------------------------------------------
-- Load Quickfix (not a command)
-------------------------------------------------------------------------------

nvimHsLspLoadQuickfix :: CommandArguments -> NeovimLsp ()
nvimHsLspLoadQuickfix arg = focusLang' False $ do
    allDiagnostics <- readContext $ view (field @"diagnosticsMap")
    curi <- getBufUri =<< nvim_get_current_buf
    let qfItems = if showAll
                  then diagnosticsToQfItems curi allDiagnostics
                  else diagnosticsToQfItems curi $
                          M.singleton curi
                          (M.findWithDefault [] curi allDiagnostics)
                  -- TODO
    replaceQfList qfItems
    if null qfItems
      then vimCommand "cclose" >> nvimEcho "nvim-hs-lsp: no diagnostics"
      else vimCommand "ccopen"
  where
    showAll = Just True == bang arg

-------------------------------------------------------------------------------
-- CodeAction
-------------------------------------------------------------------------------

nvimHsLspCodeAction :: CommandArguments -> NeovimLsp ()
nvimHsLspCodeAction _ = whenInitialized $ focusLang' False $ whenAlreadyOpened $ do
    uri <- getBufUri =<< vim_get_current_buffer
    pos <- fromNvimPos <$> getNvimPos
    let range = Record $ #start @= pos <! #end @= pos <! nil
    waitCallback $ codeAction uri range callbackCodeAction

-- HIE specific
---------------

nvimHsLspHieCaseSplit :: CommandArguments -> NeovimLsp ()
nvimHsLspHieCaseSplit _ = hiePointCommand "ghcmod:casesplit"

hiePointCommand :: String -> NeovimLsp ()
hiePointCommand cmd =  whenInitialized $ focusLang' False $ whenAlreadyOpened $ do
    uri <- getBufUri =<< vim_get_current_buffer
    pos <- fromNvimPos <$> getNvimPos
    let arg = toJSON $ #file @= uri
                    <! #pos  @= pos
                    <! nil @(Field Identity)
    void $ executeCommandRequest cmd (Some [arg]) Nothing

-- TODO haskellに限定して良い
nvimHsLspHieHsImport :: CommandArguments -> String -> NeovimLsp ()
nvimHsLspHieHsImport _ moduleToImport = focusLang' False $ do
    uri <- getBufUri =<< vim_get_current_buffer
    let arg = toJSON $ #file @= uri
                    <! #moduleToImport @= moduleToImport
                    <! nil @(Field Identity)
    void $ executeCommandRequest "hsimport:import" (Some [arg]) Nothing


-------------------------------------------------------------------------------
-- Formatting
-------------------------------------------------------------------------------

nvimHsLspFormatting :: CommandArguments -> NeovimLsp ()
nvimHsLspFormatting CommandArguments{range,bang} = whenInitialized $ focusLang' False $ whenAlreadyOpened $ do
    uri <- getBufUri =<< vim_get_current_buffer
    fopts <- readContext $ view $
                field @"lspConfig" . field @"formattingOptions"
    case (bang, range) of
      (Just True, _) ->
        waitCallback $ textDocumentFormatting uri fopts
      (_, Nothing) ->
        waitCallback $ textDocumentFormatting uri fopts
      (_, Just (l1,l2)) -> do
        let range' = Record
                   $ #start @= fromNvimPos (l1,1)
                  <! #end   @= fromNvimPos (l2,1)
                  <! nil
        waitCallback $ textDocumentRangeFormatting uri range' fopts

-------------------------------------------------------------------------------
-- References
-------------------------------------------------------------------------------

nvimHsLspReferences :: CommandArguments -> NeovimLsp ()
nvimHsLspReferences _ = whenInitialized $ focusLang' False $ whenAlreadyOpened $ do
    uri <- getBufUri =<< vim_get_current_buffer
    pos <- fromNvimPos <$> getNvimPos
    waitCallback $ textDocumentReferences uri pos callbackTextDocumentReferences

-------------------------------------------------------------------------------
-- DocumentSymbol
-------------------------------------------------------------------------------

nvimHsLspDocumentSymbol :: CommandArguments -> NeovimLsp ()
nvimHsLspDocumentSymbol _ = whenInitialized $ focusLang' False $ whenAlreadyOpened $ do
    uri <- getBufUri =<< vim_get_current_buffer
    waitCallback $ textDocumentDocumentSymbol uri

-------------------------------------------------------------------------------
-- WorkspaceSymbol
-------------------------------------------------------------------------------

nvimHsLspWorkspaceSymbol :: CommandArguments -> String -> NeovimLsp ()
nvimHsLspWorkspaceSymbol _ sym = whenInitialized $ focusLang' False $ whenAlreadyOpened $ do
    waitCallback $ workspaceSymbol sym callbackWorkspaceSymbol

-------------------------------------------------------------------------------
-- DocumentSymbol
-------------------------------------------------------------------------------

nvimHsLspRename :: CommandArguments -> String -> NeovimLsp ()
nvimHsLspRename _ newName = whenInitialized $ focusLang' False $ whenAlreadyOpened $ do
    uri <- getBufUri =<< vim_get_current_buffer
    pos <- fromNvimPos <$> getNvimPos
    waitCallback $ textDocumentRename uri pos newName

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

nvimHsLspToggleQfAutoOpen :: CommandArguments -> NeovimLsp ()
nvimHsLspToggleQfAutoOpen _ = whenInitialized $ focusLang' False $ do
    modifyContext $ field @"lspConfig"
                  . field @"autoLoadQuickfix"
                  %~ not

nvimHsLspEnableQfAutoOpen :: CommandArguments -> NeovimLsp ()
nvimHsLspEnableQfAutoOpen _ = whenInitialized $ focusLang' False $ do
    modifyContext $ field @"lspConfig"
                  . field @"autoLoadQuickfix"
                  .~ True

nvimHsLspDisableQfAutoOpen :: CommandArguments -> NeovimLsp ()
nvimHsLspDisableQfAutoOpen _ = whenInitialized $ focusLang' False $ do
    modifyContext $ field @"lspConfig"
                  . field @"autoLoadQuickfix"
                  .~ False

