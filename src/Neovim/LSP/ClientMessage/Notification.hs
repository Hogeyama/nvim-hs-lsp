
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.ClientMessage.Notification where

import           RIO
import qualified RIO.Map                      as M

import           Data.Generics.Product        (field)

import           LSP
import           Neovim
import           Neovim.LSP.Base
import           Neovim.LSP.Util

-------------------------------------------------------------------------------
-- アレ
-------------------------------------------------------------------------------

-- didOpen: newly opened text document
-- didChange: change text document
-- XXX willSave: 実際にsaveする前に送る (よくわかんない)
-- XXX willSaveWaitUntil: 実際にsaveする前に送る, save前にすべき変更が帰ってくる
-- didSave: saveした後に送る
-- didClose:

-- TextDocumentDidOpen Notification
---------------------------------------
didOpenBuffer :: (HasOutChan env, HasContext env, HasLogFunc env)
              => Buffer -> Neovim env ()
didOpenBuffer b = do
    uri      <- getBufUri b
    version  <- genUniqueVersion
    getBufLanguage b >>= \case
      Just language -> do
        contents <- getBufContents b
        let textDocumentItem = Record
                             $ #uri        @= uri
                            <! #languageId @= language
                            <! #version    @= version
                            <! #text       @= contents
                            <! nil
        sendNotification @'TextDocumentDidOpenK
             $ didOpenTextDocumentParam textDocumentItem
      Nothing -> do
        logError $ "didOpenBuffer: No language detected for "
                    <> fromString (uriToFilePath uri)

-- TextDocumentDidClose Notification
---------------------------------------
didCloseBuffer :: (HasOutChan env, HasContext env) => Buffer -> Neovim env ()
didCloseBuffer b = do
    uri <- getBufUri b
    let param = Record
              $ #textDocument @= textDocumentIdentifier uri
             <! nil
    sendNotification @'TextDocumentDidCloseK param
    resetDiagnostics uri

-- TextDocumentDidSave Notification
---------------------------------------
didSaveBuffer :: (HasOutChan env, HasContext env) => Buffer -> Neovim env ()
didSaveBuffer b = do
    uri  <- getBufUri b
    let param = Record
              $ #textDocument @= textDocumentIdentifier uri
             <! #text         @= None --Some contents
             <! nil
    sendNotification @'TextDocumentDidSaveK param
    resetDiagnostics uri

-- TextDocumentDidChange Notification
---------------------------------------
didChangeBuffer :: (HasOutChan env, HasContext env) => Buffer -> Neovim env ()
didChangeBuffer b = do
    uri      <- getBufUri b
    contents <- getBufContents b
    version  <- genUniqueVersion
    let change = Record
               $ #range       @= None
              <! #rangeLength @= None
              <! #text        @= contents
              <! nil
        param = Record
              $ #textDocument   @= versionedTextDocmentIdentifier uri version
             <! #contentChanges @= [change]
             <! nil
    sendNotification @'TextDocumentDidChangeK param
    resetDiagnostics uri

resetDiagnostics :: HasContext env => Uri -> Neovim env ()
resetDiagnostics uri =
    modifyContext $ over (field @"diagnosticsMap") $ M.delete uri

