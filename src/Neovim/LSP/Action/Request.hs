
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.Action.Request where

import           RIO
import qualified RIO.Map                  as M
import           RIO.Partial              (fromJust)

import           Data.Aeson
import           Data.Extensible
import           Control.Lens             (views)

import           Neovim
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

-- TextDocumentHover
---------------------------------------
hoverRequest :: (HasOutChan env, HasContext env)
             => Buffer
             -> NvimPos
             -> CallbackOf 'TextDocumentHoverK a
             -> Neovim env (TMVar a)
hoverRequest b p callback = do
  param <- getTextDocumentPositionParams b p
  fromJust <$> pushRequest param (Just callback)

-- TextDocumentSignatureHelp
---------------------------------------
signatureHelpRequest :: (HasOutChan env, HasContext env)
                     => Buffer
                     -> NvimPos
                     -> CallbackOf 'TextDocumentSignatureHelpK a
                     -> Neovim env (TMVar a)
signatureHelpRequest b p callback = do
  param <- getTextDocumentPositionParams b p
  fromJust <$> pushRequest param (Just callback)

-- TextDocumentDefinition
---------------------------------------
definitionRequest :: (HasOutChan env, HasContext env)
                  => Buffer
                  -> NvimPos
                  -> CallbackOf 'TextDocumentDefinitionK a
                  -> Neovim env (TMVar a)
definitionRequest b p callback = do
  param <- getTextDocumentPositionParams b p
  fromJust <$> pushRequest param (Just callback)

-- WorkspaceExecuteCommand
---------------------------------------
executeCommandRequest :: (HasOutChan env, HasContext env)
                      => String
                      -> Option [Value]
                      -> Maybe (CallbackOf 'WorkspaceExecuteCommandK a)
                      -> Neovim env (Maybe (TMVar a))
executeCommandRequest cmd margs mcallback = do
  let param = #command   @= cmd
           <! #arguments @= margs
           <! nil
  pushRequest param mcallback

-- TextDocumentCompletion
---------------------------------------
completionRequest :: (HasOutChan env, HasContext env)
                  => Buffer
                  -> NvimPos
                  -> CallbackOf 'TextDocumentCompletionK a
                  -> Neovim env (TMVar a)
completionRequest b p callback = do
  uri <- getBufUri b
  let params = #textDocument @= textDocumentIdentifier uri
            <! #position     @= nvimPosToPosition p
            <! #context      @= None
            <! nil
  fromJust <$> pushRequest params (Just callback)

-- TextDocumentCodeAction
---------------------------------------

codeAction :: (HasOutChan env, HasContext env)
           => Buffer
           -> (NvimPos, NvimPos)
           -> CallbackOf 'TextDocumentCodeActionK a
           -> Neovim env (TMVar a)
codeAction b (start,end) callback = do
  uri <- getBufUri b
  allDiagnostics <- readContext $
    views (#otherState.diagnosticsMap) (fromMaybe [] . M.lookup uri)
  let params = #textDocument @= textDocumentIdentifier uri
            <! #range        @= ( #start @= nvimPosToPosition start
                               <! #end   @= nvimPosToPosition end
                               <! nil)
            <! #context      @= context
            <! nil
      context = #diagnostics @= diags
             <! #only @= None
             <! nil
      diags = filter `flip` allDiagnostics $ \diag ->
        let range  = diag^. #range
            start' = positionToNvimPos $ range^. #start
            end'   = positionToNvimPos $ range^. #end
            line   = fst
        in line start' <= line start && line start <= line end'

  fromJust <$> pushRequest params (Just callback)


