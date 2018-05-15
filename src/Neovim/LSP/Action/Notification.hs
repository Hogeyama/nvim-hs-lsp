
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.Action.Notification
  --(
  --)
  where

import           RIO

import           Data.Extensible              (nil, (<!), (@=))
import           Data.Maybe                   (fromJust)
import qualified Data.Map                     as M

import           Neovim
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

-- didOpen: newly opened text document
-- didChange: change text document
-- XXX willSave: 実際にsaveする前に送る (よくわかんない)
-- XXX willSaveWaitUntil: 実際にsaveする前に送る, save前にすべき変更が帰ってくる
-- didSave: saveした後に送る
-- didClose:

-- TextDocumentDidOpen Notification
---------------------------------------
didOpenBuffer :: (HasOutChan' env, HasContext' env, HasLoggerName' env)
              => Buffer -> Neovim env ()
didOpenBuffer b = do
    uri      <- getBufUri b
    version  <- genUniqueVersion
    language <- fromJust <$> getBufLanguage b
    contents <- getBufContents b
    let textDocumentItem :: TextDocumentItem
                         = #uri        @= uri
                        <! #languageId @= language
                        <! #version    @= version
                        <! #text       @= contents
                        <! nil
    push $ notification @'TextDocumentDidOpenK
         $ didOpenTextDocumentParam textDocumentItem

-- TextDocumentDidClose Notification
---------------------------------------
didCloseBuffer :: (HasOutChan' env, HasContext' env) => Buffer -> Neovim env ()
didCloseBuffer b = do
    uri <- getBufUri b
    let param = #textDocument @= textDocumentIdentifier uri
             <! nil
    push $ notification @'TextDocumentDidCloseK param
    resetDiagnostics uri

-- TextDocumentDidSave Notification
---------------------------------------
didSaveBuffer :: (HasOutChan' env, HasContext' env) => Buffer -> Neovim env ()
didSaveBuffer b = do
    uri  <- getBufUri b
    let param = #textDocument @= textDocumentIdentifier uri
             <! #text         @= None --Some contents
             <! nil
    push $ notification @'TextDocumentDidSaveK param
    resetDiagnostics uri

-- TextDocumentDidChange Notification
---------------------------------------
didChangeBuffer :: (HasOutChan' env, HasContext' env) => Buffer -> Neovim env ()
didChangeBuffer b = do
    uri      <- getBufUri b
    contents <- getBufContents b
    version  <- genUniqueVersion
    let change = #range       @= None
              <! #rangeLength @= None
              <! #text        @= contents
              <! nil
        param = #textDocument   @= versionedTextDocmentIdentifier uri version
             <! #contentChanges @= [change]
             <! nil
    push $ notification @'TextDocumentDidChangeK param
    resetDiagnostics uri

resetDiagnostics :: HasContext' env => Uri -> Neovim env ()
resetDiagnostics uri =
  modifyContext $ over (lspOtherState.diagnosticsMap) $ M.delete uri

