
{-# LANGUAGE TemplateHaskell #-}

module Neovim.LSP (plugin) where

import RIO
import Neovim
import Neovim.LSP.Base
import Neovim.LSP.Plugin

plugin :: Neovim () NeovimPlugin
plugin = do
    logFile <- tryAny (nvim_get_var "NvimHsLsp_logFile" >>= fromObject') >>= \case
      Right file -> return file
      _ -> return "/tmp/nvim-hs-lsp.log"
    initialEnv <- initialEnvM logFile
    wrapPlugin Plugin
      { environment = initialEnv
      , exports =
              [
              -- Notification
                $(command' 'nvimHsLspInitialize)      [ "async"           ]
              , $(command' 'nvimHsLspStartServer)     [ "async"           ]
              , $(command' 'nvimHsLspOpenBuffer)      [ "async","!"       ]
              , $(command' 'nvimHsLspCloseBuffer)     [ "async"           ]
              , $(command' 'nvimHsLspChangeBuffer)    [ "async","!"       ]
              , $(command' 'nvimHsLspSaveBuffer)      [ "async","!"       ]
              , $(command' 'nvimHsLspExit)            [ "async"           ]
              , $(command' 'nvimHsLspStopServer)      [ "async"           ]
              -- Request
              , $(command' 'nvimHsLspHover)           [ "async"           ]
              , $(command' 'nvimHsLspInfo)            [ "async"           ]
              , $(command' 'nvimHsLspDefinition)      [ "async"           ]
              , $(command' 'nvimHsLspCodeAction)      [                   ]
              , $(command' 'nvimHsLspFormatting)      [ "async", "!", "%" ]
              , $(command' 'nvimHsLspReferences)      [ "async"           ]
              , $(command' 'nvimHsLspHieCaseSplit)    [ "async"           ]
              , $(command' 'nvimHsLspDocumentSymbol)  [ "async"           ]
              , $(command' 'nvimHsLspWorkspaceSymbol) [ "async"           ]
              , $(command' 'nvimHsLspRename)          [ "sync"            ]
              , $(command' 'nvimHsLspHieHsImport)     [ "sync"            ]
              -- Other
              , $(command' 'nvimHsLspLoadQuickfix)    [ "async", "!"      ]
              --
              , $(function' 'nvimHsLspComplete)       Sync
              , $(function' 'nvimHsLspAsyncComplete)  Async
              ]
      }

