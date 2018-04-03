
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Neovim.LSP (plugin) where

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
              $(command' 'nvimHsLspInitialize)     ["async"]
            , $(command' 'nvimHsLspOpenBuffer)     ["async","!"]
            , $(command' 'nvimHsLspCloseBuffer)    ["async"]
            , $(command' 'nvimHsLspChangeBuffer)   ["async","!"]
            , $(command' 'nvimHsLspSaveBuffer)     ["async","!"]
            , $(command' 'nvimHsLspExit)           ["async"]
            -- Request
            , $(command' 'nvimHsLspHover)          ["async"]
            , $(command' 'nvimHsLspInfo)           ["async"]
            , $(command' 'nvimHsLspDefinition)     ["async"]
            , $(command' 'nvimHsLspApplyRefactOne) ["async"]
            --
            , $(function' 'nvimHsLspComplete)      Sync
            , $(function' 'nvimHsLspAsyncComplete) Async
            ]
    }



