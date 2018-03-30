
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall         #-}

module Neovim.LSP.Action.Request where

import           Data.Aeson
import           Data.Extensible
import           UnliftIO

import           Neovim
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

-- TextDocumentHover
---------------------------------------
hoverRequest :: (HasOutChan' env, HasContext' env)
             => Buffer
             -> NvimPos
             -> Maybe (CallbackOf 'TextDocumentHoverK a)
             -> Neovim env (Maybe (TMVar a))
hoverRequest b p mcallback = do
  param <- getTextDocumentPositionParams b p
  pushRequest param mcallback

-- TextDocumentSignatureHelp
---------------------------------------
signatureHelpRequest :: (HasOutChan' env, HasContext' env)
                     => Buffer
                     -> NvimPos
                     -> Maybe (CallbackOf 'TextDocumentSignatureHelpK a)
                     -> Neovim env (Maybe (TMVar a))
signatureHelpRequest b p mcallback = do
  param <- getTextDocumentPositionParams b p
  pushRequest param mcallback

-- TextDocumentDefinition
---------------------------------------
definitionRequest :: (HasOutChan' env, HasContext' env)
                  => Buffer
                  -> NvimPos
                  -> Maybe (CallbackOf 'TextDocumentDefinitionK a)
                  -> Neovim env (Maybe (TMVar a))
definitionRequest b p mcallback = do
  param <- getTextDocumentPositionParams b p
  pushRequest param mcallback

-- WorkspaceExecuteCommand
---------------------------------------
executeCommandRequest :: (HasOutChan' env, HasContext' env)
                      => String
                      -> Option [Value]
                      -> Maybe (CallbackOf 'WorkspaceExecuteCommandK a)
                      -> Neovim env (Maybe (TMVar a))
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
                  -> Maybe (CallbackOf 'TextDocumentCompletionK a)
                  -> Neovim env (Maybe (TMVar a))
completionRequest b p callback = do
  uri <- getBufUri b
  let params = #textDocument @= textDocumentIdentifier uri
            <: #position     @= nvimPosToPosition p
            <: #context      @= None
            <: nil
  pushRequest params callback

