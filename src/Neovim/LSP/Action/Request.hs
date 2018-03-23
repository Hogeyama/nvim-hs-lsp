
{-# LANGUAGE DataKinds        #-}
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
hoverRequest :: (HasOutChannel r, HasContext r)
             => Buffer -> NvimPos -> Neovim r st ()
hoverRequest b p = pushRequest @'TextDocumentHoverK
  =<< getTextDocumentPositionParams b p

-- TextDocumentSignatureHelp
---------------------------------------
signatureHelpRequest :: (HasOutChannel r, HasContext r)
                     => Buffer -> NvimPos -> Neovim r st ()
signatureHelpRequest b p = pushRequest @'TextDocumentSignatureHelpK
  =<< getTextDocumentPositionParams b p

-- TextDocumentDefinition
---------------------------------------
definitionRequest :: (HasOutChannel r, HasContext r)
                  => Buffer -> NvimPos -> Neovim r st ()
definitionRequest b p = pushRequest @'TextDocumentDefinitionK
  =<< getTextDocumentPositionParams b p


-- WorkspaceExecuteCommand
---------------------------------------
executeCommandRequest :: (HasOutChannel r, HasContext r)
                      => String -> Option [Value] -> Neovim r st ()
executeCommandRequest cmd margs = pushRequest @'WorkspaceExecuteCommandK $
     #command   @= cmd
  <: #arguments @= margs
  <: nil

-- TextDocumentCompletion
---------------------------------------
completionRequest :: (HasOutChannel r, HasContext r)
                  =>  Buffer -> NvimPos -> Neovim r st ()
completionRequest b p = do
  uri <- getBufUri b
  let params = #textDocument @= textDocumentIdentifier uri
            <: #position     @= nvimPosToPosition p
            <: #context      @= None
            <: nil
  pushRequest @'TextDocumentCompletionK params

