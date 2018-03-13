
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Neovim.LSP.Plugin where

import           Control.Monad (void)

import           Neovim

import           Neovim.LSP.Base
import           Neovim.LSP.Util
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Action.Request
import           Neovim.LSP.Action.Notification
import           Neovim.LSP.LspPlugin.Notification (notificationHandler)
import           Neovim.LSP.LspPlugin.Request      (requestHandler)
import           Neovim.LSP.LspPlugin.Response     (responseHandler)

nvimHsLspInitialize :: CommandArguments -> NeovimLsp ()
nvimHsLspInitialize _ = do
  initializeLsp "hie" ["--lsp", "-d", "-l", "/tmp/LanguageServer.log"]
  void $ dispatcher [notificationHandler, requestHandler, responseHandler]
  cwd <- filePathToUri <$> errOnInvalidResult (vim_call_function "getcwd" [])
  pushRequest @'InitializeK (initializeParam Nothing (Just cwd))
  debugM . show =<< vim_command_output "au BufRead,BufNewFile *.hs NvimHsLspOpenBuffer"
  debugM . show =<< vim_command_output "au InsertLeave *.hs NvimHsLspChangeBuffer"
  debugM . show =<< vim_command_output "au BufWrite *.hs NvimHsLspSaveBuffer"
  void $ vim_out_write $ "nvim-hs-lsp: Initialized" ++ "\n"

nvimHsLspOpenBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspOpenBuffer _ = didOpenBuffer =<< vim_get_current_buffer'

nvimHsLspChangeBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspChangeBuffer _ = didChangeBuffer =<< vim_get_current_buffer'

nvimHsLspSaveBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspSaveBuffer _ = didSaveBuffer =<< vim_get_current_buffer'

nvimHsLspHoverRequest :: CommandArguments -> NeovimLsp ()
nvimHsLspHoverRequest _ = do
  b <- vim_get_current_buffer'
  [_bufnum, lnum, col, _off] <-
      errOnInvalidResult $ vim_call_function "getpos" [ObjectString "."]
  hoverRequest b (lnum,col)

nvimHsLspExit :: CommandArguments -> NeovimLsp ()
nvimHsLspExit _ = push $ notification @'ExitK exitParam

