
{-# OPTIONS_GHC -Wall            #-}

module Neovim.LSP.Plugin where

import           RIO

import           Control.Monad                     (unless, void, when)
import           Control.Monad.Extra               (ifM, whenJust)
import           Data.Aeson                        hiding (Object)
import qualified Data.ByteString.Char8             as B
import           Data.Char                         (isAlphaNum)
import           Data.List                         (isPrefixOf, partition)
import qualified Data.Map                          as M

import           Neovim                            hiding (whenM, unlessM, (<>))

import           Neovim.LSP.Action.Notification
import           Neovim.LSP.Action.Request
import           Neovim.LSP.Action.Callback
import           Neovim.LSP.Base
import           Neovim.LSP.LspPlugin.Callback
import           Neovim.LSP.LspPlugin.Notification (notificationHandler)
import           Neovim.LSP.LspPlugin.Request      (requestHandler)
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

-------------------------------------------------------------------------------
-- Initialize
-------------------------------------------------------------------------------

nvimHsLspInitialize :: CommandArguments -> NeovimLsp ()
nvimHsLspInitialize _ = loggingError $ do
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
            initializeLsp cmd args
            #fileType .== Just ft
            dispatch [notificationHandler, requestHandler, callbackHandler]
            cwd <- getCWD
            pushRequest' @'InitializeK (initializeParam Nothing (Just cwd))
            let pat = def { acmdPattern = "*" }
                arg = def { bang = Just True }
            Just Right{} <- addAutocmd "BufRead,BufNewFile"
                              pat (nvimHsLspOpenBuffer arg)
            Just Right{} <- addAutocmd "TextChanged,TextChangedI"
                              pat (nvimHsLspChangeBuffer arg)
            Just Right{} <- addAutocmd "BufWrite"
                              pat (nvimHsLspSaveBuffer arg)
            nvimHsLspOpenBuffer def
            whenM (readContext . view $ #lspConfig.autoLoadQuickfix) $
              vim_command' "botright copen"
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
-- Notification
-------------------------------------------------------------------------------

whenAlreadyOpened' :: Bool -> NeovimLsp () -> NeovimLsp ()
whenAlreadyOpened' silent m = do
  uri <- getBufUri =<< vim_get_current_buffer'
  ifM (alreadyOpened uri) m (unless silent $ vim_out_write' "nvim-hs-lsp: Not opened yet\n")

whenAlreadyOpened :: NeovimLsp () -> NeovimLsp ()
whenAlreadyOpened = whenAlreadyOpened' False

alreadyOpened :: Uri -> NeovimLsp Bool
alreadyOpened uri = M.member uri <$> useTV #openedFiles

nvimHsLspOpenBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspOpenBuffer arg = whenInitialized' silent $ do
  b   <- vim_get_current_buffer'
  mft <- getBufLanguage b
  whenJust mft $ \ft -> do
    serverFT <- useTV #fileType
    when (Just ft == serverFT) $ do
      uri <- getBufUri b
      unlessM (alreadyOpened uri) $ do
        #openedFiles %== M.insert uri 0
        didOpenBuffer b
  where silent = Just True == bang arg

nvimHsLspCloseBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspCloseBuffer _ = whenInitialized $ do
  b <- vim_get_current_buffer'
  uri <- getBufUri b
  ifM (not <$> alreadyOpened uri) (vim_out_write' "nvim-hs-lsp: Not opened yet\n") $ do
    #openedFiles %== M.delete uri
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
  push $ notification @'ExitK exitParam
  finalizeLSP

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

-- Hover
--------
nvimHsLspInfo :: CommandArguments -> NeovimLsp ()
nvimHsLspInfo _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ hoverRequest b pos callbackHoverOneLine

nvimHsLspHover :: CommandArguments -> NeovimLsp ()
nvimHsLspHover _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ hoverRequest b pos callbackHoverPreview

-- Definition
-------------
nvimHsLspDefinition :: CommandArguments -> NeovimLsp ()
nvimHsLspDefinition _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ definitionRequest b pos callbackDefinition

-- HIE ApplyRefact
------------------

-- argument: {file: Uri, start_pos: Position}
nvimHsLspApplyRefactOne :: CommandArguments -> NeovimLsp ()
nvimHsLspApplyRefactOne _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  uri <- getBufUri b
  pos <- getNvimPos
  let  arg = toJSON $ object [ "file" .= uri
                             , "start_pos" .= nvimPosToPosition pos
                             ]
  void $ executeCommandRequest "applyrefact:applyOne" (Some [arg]) Nothing

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
            | Just n <- readMaybe (B.unpack s) -> n
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
      let sorted = uncurry (++) $ partition (isPrefixOf base . view #word)  xs
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
-- ?
-------------------------------------------------------------------------------

nvimHsLspLoadQuickfix :: CommandArguments -> NeovimLsp ()
nvimHsLspLoadQuickfix arg = do
    allDiagnostics <- readContext $ view (#otherState.diagnosticsMap)
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
      else vim_command' "botright copen"
  where
    showAll = Just True == bang arg

