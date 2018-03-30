{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wall          #-}

module Neovim.LSP.Plugin where

import           UnliftIO
import           Control.Lens                      (view)
import           Control.Monad                     (void)
import           Control.Monad.Extra               (ifM)
import           Data.Aeson                        hiding (Object)
import qualified Data.ByteString.Char8             as B
import           Data.Char (isAlphaNum)
import           Data.List (isPrefixOf, partition)
import qualified Data.Map                          as M

import           Neovim
import           Neovim.Context                    (restart)

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
nvimHsLspInitialize ca = loggingError $ do
  initialized <- isInitialized
  if initialized then do
    vim_out_write' $ "nvim-hs-lsp: Already initialized" ++ "\n"
  else do
    b <- getBufLanguage =<< vim_get_current_buffer'
    if b /= "haskell"
      then initializeLsp "rustup" ["run", "nightly", "rls"]
      else initializeLsp "hie" ["--lsp", "-d", "-l", "/tmp/LanguageServer.log"]
    dispatch [notificationHandler, requestHandler, callbackHandler]
    cwd <- filePathToUri <$> errOnInvalidResult (vim_call_function "getcwd" [])
    pushRequest' @'InitializeK (initializeParam Nothing (Just cwd))
    let pat = def { acmdPattern = "*\\.\\(hs\\|rs\\)" }
    Just Right{} <- addAutocmd "BufRead,BufNewFile"       pat (nvimHsLspOpenBuffer ca)
    Just Right{} <- addAutocmd "TextChanged,TextChangedI" pat (nvimHsLspChangeBuffer ca)
    Just Right{} <- addAutocmd "BufWrite"                 pat (nvimHsLspSaveBuffer ca)
    vim_out_write' $ "nvim-hs-lsp: Initialized" ++ "\n"
    nvimHsLspOpenBuffer ca
    void $ vim_command_output "botright copen"

whenInitialized :: NeovimLsp () -> NeovimLsp ()
whenInitialized m = isInitialized >>= \case
  True  -> m
  False -> vim_out_write' $ "nvim-hs-lsp: Not initialized!" ++ "\n"

-------------------------------------------------------------------------------
-- Notification
-------------------------------------------------------------------------------

whenAlreadyOpened :: NeovimLsp () -> NeovimLsp ()
whenAlreadyOpened m = do
  uri <- getBufUri =<< vim_get_current_buffer'
  ifM (alreadyOpened uri) m (vim_out_write' "nvim-hs-lsp: Not opened yet\n")

alreadyOpened :: Uri -> NeovimLsp Bool
alreadyOpened uri = M.member uri <$> useTV openedFiles

nvimHsLspOpenBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspOpenBuffer _ = whenInitialized $ do
  b <- vim_get_current_buffer'
  uri <- getBufUri b
  ifM (alreadyOpened uri) (vim_out_write' "nvim-hs-lsp: Already opened\n") $ do
    openedFiles %== M.insert uri 0
    didOpenBuffer b

nvimHsLspCloseBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspCloseBuffer _ = whenInitialized $ do
  b <- vim_get_current_buffer'
  uri <- getBufUri b
  ifM (not <$> alreadyOpened uri) (vim_out_write' "nvim-hs-lsp: Not opened yet\n") $ do
    openedFiles %== M.delete uri
    didCloseBuffer b

nvimHsLspChangeBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspChangeBuffer _ = whenInitialized $ whenAlreadyOpened $
  didChangeBuffer =<< vim_get_current_buffer'

nvimHsLspSaveBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspSaveBuffer _ = whenInitialized $ whenAlreadyOpened $
  didSaveBuffer =<< vim_get_current_buffer'

nvimHsLspExit :: CommandArguments -> NeovimLsp ()
nvimHsLspExit _ = whenInitialized $ do
  push $ notification @'ExitK exitParam
  finalizeLSP
  restart

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

nvimHsLspInfo :: CommandArguments -> NeovimLsp ()
nvimHsLspInfo _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ hoverRequest b pos (Just callbackHoverOneLine)

nvimHsLspInfoDetail :: CommandArguments -> NeovimLsp ()
nvimHsLspInfoDetail _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ hoverRequest b pos (Just callbackHover)

nvimHsLspHover :: CommandArguments -> NeovimLsp ()
nvimHsLspHover = nvimHsLspInfoDetail

nvimHsLspDefinition :: CommandArguments -> NeovimLsp ()
nvimHsLspDefinition _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ definitionRequest b pos (Just callbackDefinition)

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

nvimHsLspComplete :: Object -> String
                  -> NeovimLsp (Either Int [VimCompleteItem])
nvimHsLspComplete findstart base = do
  b <- vim_get_current_buffer'
  (line, col) <- getNvimPos
  let findStart = case findstart of
        ObjectInt n -> n
        ObjectString s -> read (B.unpack s)
        _ -> error "流石にここに来たら怒っていいよね"
  if findStart == 1 then do
    s <- nvim_get_current_line'
    let foo = dropWhile isAlphaNum $ reverse $ take col s
    let len = length foo
    debugM $ "foo = " ++ show foo
    otherState.completionOffset .== Just len
    return (Left len)
  else do
    Just len <- useTV (otherState.completionOffset)
    otherState.completionOffset .== Nothing
    Just var <- completionRequest b (line,col+len) (Just callbackComplete)
    xs <- atomically (takeTMVar var)
    debugM $ "COMPLETION: base = " ++ show base
    let sorted = uncurry (++) $ partition (isPrefixOf base . view #word)  xs
    return (Right sorted)

