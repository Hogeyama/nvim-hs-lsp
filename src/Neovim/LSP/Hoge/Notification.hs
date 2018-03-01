
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.Hoge.Notification
  --(
  --)
  where

import           Data.Extensible               ((@=), (<:), nil)
import           Neovim
import           Neovim.LSP.Base
import           Neovim.LSP.Util
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type

-- didOpen: newly opened text document
-- didChange: change text document
-- XXX willSave: 実際にsaveする前に送る (よくわかんない)
-- XXX willSaveWaitUntil: 実際にsaveする前に送る, save前にすべき変更が帰ってくる
-- didSave: saveした後に送る
-- didClose:

-- TextDocumentDidOpen Notification
---------------------------------------
didOpenBuffer :: Buffer -> Neovim HandlerConfig st ()
didOpenBuffer b = do
    contents <- getBufContents b
    language <- getBufLanguage b
    uri      <- getBufUri b
    version  <- genUniqueVersion
    let textDocumentItem = #uri        @= uri
                        <: #languageId @= language
                        <: #version    @= version
                        <: #text       @= contents
                        <: nil
    push $ notification @'TextDocumentDidOpenK
         $ didOpenTextDocumentParam textDocumentItem

-- TextDocumentDidSave Notification
---------------------------------------
didSaveBuffer :: Buffer -> Neovim HandlerConfig st ()
didSaveBuffer b = do
    tid      <- textDocumentIdentifier <$> getBufUri b
    contents <- getBufContents b
    let param = #textDocument @= tid
             <: #text         @= Some contents
             <: nil
    push $ notification @'TextDocumentDidSaveK param

-- TextDocumentDidChange Notification
---------------------------------------
didChangeBuffer :: Buffer -> Neovim HandlerConfig st ()
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

