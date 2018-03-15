
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.Plugin where

import           Control.Lens                      (use)
import           Control.Lens.Operators
import           Control.Monad                     (void)
import           Control.Monad.Extra               (ifM)
import qualified Data.Map                          as M

import           Neovim
import           Neovim.Context                    (restart)

import           Neovim.LSP.Base
import           Neovim.LSP.Util
import           Neovim.LSP.Action.Notification
import           Neovim.LSP.Action.Request
import           Neovim.LSP.LspPlugin.Notification (notificationHandler)
import           Neovim.LSP.LspPlugin.Request      (requestHandler)
import           Neovim.LSP.LspPlugin.Response     (responseHandler)
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type
import Control.Concurrent (threadDelay)

nvimHsLspInitialize :: CommandArguments -> NeovimLsp ()
nvimHsLspInitialize _ = do
  initialized <- isInitialized
  if initialized then do
    vim_out_write' $ "nvim-hs-lsp: Already initialized" ++ "\n"
  else do
    initializeLsp "hie" ["--lsp", "--vomit", "--ekg", "-d", "-l", "/tmp/LanguageServer.log"]
    void $ dispatcher [notificationHandler, requestHandler, responseHandler]
    cwd <- filePathToUri <$> errOnInvalidResult (vim_call_function "getcwd" [])
    pushRequest @'InitializeK (initializeParam Nothing (Just cwd))
    debugM . show =<< vim_command_output "au BufRead,BufNewFile *.hs NvimHsLspOpenBuffer"
    debugM . show =<< vim_command_output "au TextChanged,TextChangedI *.hs NvimHsLspChangeBuffer"
    --debugM . show =<< vim_command_output "au BufWrite *.hs NvimHsLspChangeBuffer"
    --debugM . show =<< vim_command_output "au BufWrite *.hs NvimHsLspSaveBuffer"

    vim_out_write' $ "nvim-hs-lsp: Initialized" ++ "\n"
    nvimHsLspOpenBuffer undefined
    debugM . show =<< vim_command_output "botright copen"

whenInitialized :: NeovimLsp () -> NeovimLsp ()
whenInitialized m = isInitialized >>= \case
  True  -> m
  False -> vim_out_write' $ "nvim-hs-lsp: Not initialized!" ++ "\n"

whenAlreadyOpened :: NeovimLsp () -> NeovimLsp ()
whenAlreadyOpened m = do
  uri <- getBufUri =<< vim_get_current_buffer'
  ifM (alreadyOpened uri) m (vim_out_write' "nvim-hs-lsp: Not opened yet\n")

alreadyOpened :: Uri -> NeovimLsp Bool
alreadyOpened uri = do
  opened <- use openedFiles
  return $ M.member uri opened

nvimHsLspOpenBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspOpenBuffer _ = whenInitialized $ do
  b <- vim_get_current_buffer'
  uri <- getBufUri b
  ifM (alreadyOpened uri) (vim_out_write' "nvim-hs-lsp: Already opened\n") $ do
    openedFiles %= M.insert uri ()
    didOpenBuffer b

nvimHsLspCloseBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspCloseBuffer _ = whenInitialized $ do
  b <- vim_get_current_buffer'
  uri <- getBufUri b
  ifM (not <$> alreadyOpened uri) (vim_out_write' "nvim-hs-lsp: Not opened yet\n") $ do
    openedFiles %= M.delete uri
    didCloseBuffer b

nvimHsLspChangeBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspChangeBuffer _ = whenInitialized $ whenAlreadyOpened $
  didChangeBuffer =<< vim_get_current_buffer'

nvimHsLspSaveBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspSaveBuffer _ = whenInitialized $ whenAlreadyOpened $
  didSaveBuffer =<< vim_get_current_buffer'

nvimHsLspHoverRequest :: CommandArguments -> NeovimLsp ()
nvimHsLspHoverRequest _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  [_bufnum, lnum, col, _off] <-
      errOnInvalidResult $ vim_call_function "getpos" [ObjectString "."]
  hoverRequest b (lnum,col)

nvimHsLspExit :: CommandArguments -> NeovimLsp ()
nvimHsLspExit _ = whenInitialized $ do
  push $ notification @'ExitK exitParam
  restart

