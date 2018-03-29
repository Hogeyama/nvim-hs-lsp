
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.Action.Notification
  --(
  --)
  where

import           Data.Extensible              (nil, (<:), (@=))
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
    contents <- getBufContents b
    language <- getBufLanguage b
    uri      <- getBufUri b
    version  <- genUniqueVersion
    let textDocumentItem :: TextDocumentItem
                         = #uri        @= uri
                        <: #languageId @= language
                        <: #version    @= version
                        <: #text       @= contents
                        <: nil
    push $ notification @'TextDocumentDidOpenK
         $ didOpenTextDocumentParam textDocumentItem

-- TextDocumentDidClose Notification
---------------------------------------
didCloseBuffer :: (HasOutChan' env, HasContext' env) => Buffer -> Neovim env ()
didCloseBuffer b = do
    tid      <- textDocumentIdentifier <$> getBufUri b
    let param = #textDocument @= tid <: nil
    push $ notification @'TextDocumentDidCloseK param

-- TextDocumentDidSave Notification
---------------------------------------
didSaveBuffer :: (HasOutChan' env) => Buffer -> Neovim env ()
didSaveBuffer b = do
    tid      <- textDocumentIdentifier <$> getBufUri b
    --contents <- getBufContents b
    let param = #textDocument @= tid
             <: #text         @= None --Some contents
             <: nil
    push $ notification @'TextDocumentDidSaveK param

-- TextDocumentDidChange Notification
---------------------------------------
didChangeBuffer :: (HasOutChan' env, HasContext' env) => Buffer -> Neovim env ()
didChangeBuffer b = do
    contents <- getBufContents b
    vtid <- versionedTextDocmentIdentifier <$> getBufUri b <*> genUniqueVersion
    let change = #range       @= None
              <: #rangeLength @= None
              <: #text        @= contents
              <: nil
        param = #textDocument   @= vtid
             <: #contentChanges @= [change]
             <: nil
    push $ notification @'TextDocumentDidChangeK param

