
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
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
             => Buffer -> NvimPos -> Neovim env ()
hoverRequest b p = pushRequest @'TextDocumentHoverK
  =<< getTextDocumentPositionParams b p

-- TextDocumentSignatureHelp
---------------------------------------
signatureHelpRequest :: (HasOutChan' env, HasContext' env)
                     => Buffer -> NvimPos -> Neovim env ()
signatureHelpRequest b p = pushRequest @'TextDocumentSignatureHelpK
  =<< getTextDocumentPositionParams b p

-- TextDocumentDefinition
---------------------------------------
definitionRequest :: (HasOutChan' env, HasContext' env)
                  => Buffer -> NvimPos -> Neovim env ()
definitionRequest b p = pushRequest @'TextDocumentDefinitionK
  =<< getTextDocumentPositionParams b p


-- WorkspaceExecuteCommand
---------------------------------------
executeCommandRequest :: (HasOutChan' env, HasContext' env)
                      => String -> Option [Value] -> Neovim env ()
executeCommandRequest cmd margs = pushRequest @'WorkspaceExecuteCommandK $
     #command   @= cmd
  <: #arguments @= margs
  <: nil

-- TextDocumentCompletion
---------------------------------------
completionRequest :: (HasOutChan' env, HasContext' env)
                  =>  Buffer -> NvimPos -> Neovim env ()
completionRequest b p = do
  uri <- getBufUri b
  let params = #textDocument @= textDocumentIdentifier uri
            <: #position     @= nvimPosToPosition p
            <: #context      @= None
            <: nil
  pushRequest @'TextDocumentCompletionK params

