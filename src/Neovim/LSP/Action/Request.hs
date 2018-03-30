
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall         #-}

module Neovim.LSP.Action.Request where

import           Data.Aeson
import           Data.Extensible

import           Neovim
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

-- TextDocumentHover
---------------------------------------
hoverRequest :: (HasOutChan' env, HasContext' env)
             => Buffer
             -> NvimPos
             -> Maybe (CallbackOf 'TextDocumentHoverK)
             -> Neovim env ()
hoverRequest b p callback = do
  param <- getTextDocumentPositionParams b p
  pushRequest param callback

-- TextDocumentSignatureHelp
---------------------------------------
signatureHelpRequest :: (HasOutChan' env, HasContext' env)
                     => Buffer
                     -> NvimPos
                     -> Maybe (CallbackOf 'TextDocumentSignatureHelpK)
                     -> Neovim env ()
signatureHelpRequest b p callback = do
  param <- getTextDocumentPositionParams b p
  pushRequest param callback

-- TextDocumentDefinition
---------------------------------------
definitionRequest :: (HasOutChan' env, HasContext' env)
                  => Buffer
                  -> NvimPos
                  -> Maybe (CallbackOf 'TextDocumentDefinitionK)
                  -> Neovim env ()
definitionRequest b p callback = do
  param <- getTextDocumentPositionParams b p
  pushRequest param callback

-- WorkspaceExecuteCommand
---------------------------------------
executeCommandRequest :: (HasOutChan' env, HasContext' env)
                      => String
                      -> Option [Value]
                      -> Maybe (CallbackOf 'WorkspaceExecuteCommandK)
                      -> Neovim env ()
executeCommandRequest cmd margs callback = do
  let param = #command   @= cmd
           <: #arguments @= margs
           <: nil
  pushRequest param callback

-- TextDocumentCompletion
---------------------------------------
completionRequest :: (HasOutChan' env, HasContext' env)
                  => Buffer
                  -> NvimPos
                  -> Maybe (CallbackOf 'TextDocumentCompletionK)
                  -> Neovim env ()
completionRequest b p callback = do
  uri <- getBufUri b
  let params = #textDocument @= textDocumentIdentifier uri
            <: #position     @= nvimPosToPosition p
            <: #context      @= None
            <: nil
  pushRequest params callback

