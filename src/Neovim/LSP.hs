
{-# LANGUAGE TemplateHaskell #-}

module Neovim.LSP (plugin) where

import RIO
import Neovim
import Neovim.LSP.Base        hiding (Plugin)
import Neovim.LSP.Plugin

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
  initialEnv <- initialEnvM
  wrapPlugin Plugin
    { environment = initialEnv
    , exports =
            [
            -- Notification
              $(command' 'nvimHsLspInitialize) ["async"]
            , $(command' 'nvimHsLspOpenBuffer) ["async","!"]
            , $(command' 'nvimHsLspCloseBuffer) ["async"]
            , $(command' 'nvimHsLspChangeBuffer) ["async","!"]
            , $(command' 'nvimHsLspSaveBuffer) ["async","!"]
            , $(command' 'nvimHsLspExit) ["async"]
            -- Request
            , $(command' 'nvimHsLspHover) ["async"]
            , $(command' 'nvimHsLspInfo) ["async"]
            , $(command' 'nvimHsLspDefinition) ["async"]
            , $(command' 'nvimHsLspCodeAction) ["async"]
            , $(command' 'nvimHsLspHieCaseSplit) ["async"]
            , $(command' 'nvimHsLspFormatting) ["async", "!", "%"]
            , $(command' 'nvimHsLspReferences) ["async"]
            -- Other
            , $(command' 'nvimHsLspLoadQuickfix) ["async", "!"]
            --
            , $(function' 'nvimHsLspComplete) Sync
            , $(function' 'nvimHsLspAsyncComplete) Async
            , $(function' 'nvimHsLspHieHsImport) Sync
            ]
    }

