
module Neovim.LSP.Plugin where

import           Neovim

import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type ()
import           Neovim.LSP.Hoge.Notification


type NeovimLSP = Neovim HandlerConfig LSPState

nvimHsLspInitialize :: CommandArguments -> NeovimLSP ()
nvimHsLspInitialize _ = do
  initializeLSP "hie" ["--lsp", "-d", "-l", "/tmp/LanguageServer.log"]
  return ()

nvimHsLspOpen :: CommandArguments -> NeovimLSP ()
nvimHsLspOpen _ = didOpenBuffer =<< vim_get_current_buffer'



