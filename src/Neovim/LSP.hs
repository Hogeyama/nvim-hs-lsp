
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Neovim.LSP (plugin) where

import Neovim
import Neovim.LSP.Base        hiding (Plugin)
import Neovim.LSP.Plugin
import Control.Monad.IO.Class (liftIO)

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = do
  initialEnv <- liftIO initialEnvIO
  wrapPlugin Plugin
    { exports = []
    , statefulExports =
      [ StatefulFunctionality
          { readOnly = initialEnv
          , writable = initialState
          , functionalities =
            [
            -- Notification
              $(command' 'nvimHsLspInitialize)        ["async"]
            , $(command' 'nvimHsLspOpenBuffer)        ["async"]
            , $(command' 'nvimHsLspCloseBuffer)       ["async"]
            , $(command' 'nvimHsLspChangeBuffer)      ["async"]
            , $(command' 'nvimHsLspSaveBuffer)        ["async"]
            , $(command' 'nvimHsLspExit)              ["async"]
            -- Request
            , $(command' 'nvimHsLspHoverRequest)      ["async"]
            , $(command' 'nvimHsLspDefinitionRequest) ["async"]
            ]
        }
      ]
    }



