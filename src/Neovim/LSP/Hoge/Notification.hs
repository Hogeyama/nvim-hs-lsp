
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
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
import qualified Neovim.LSP.Protocol.Type.Key        as K

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
    let textDocumentItem = K.uri        @= uri
                        <: K.languageId @= language
                        <: K.version    @= version
                        <: K.text       @= contents
                        <: nil
    push $ notification @'TextDocumentDidOpenK
         $ didOpenTextDocumentParam textDocumentItem

-- TextDocumentDidSave Notification
---------------------------------------
didSaveBuffer :: Buffer -> Neovim HandlerConfig st ()
didSaveBuffer b = do
    tid      <- textDocumentIdentifier <$> getBufUri b
    contents <- getBufContents b
    let param = K.textDocument @= tid
             <: K.text         @= Some contents
             <: nil
    push $ notification @'TextDocumentDidSaveK param

-- TextDocumentDidChange Notification
---------------------------------------
didChangeBuffer :: Buffer -> Neovim HandlerConfig st ()
didChangeBuffer b = do
    contents <- getBufContents b
    vtid <- versionedTextDocmentIdentifier <$> getBufUri b <*> genUniqueVersion
    let change = K.range       @= None
              <: K.rangeLength @= None
              <: K.text        @= contents
              <: nil
        param = K.textDocument   @= vtid
             <: K.contentChanges @= [change]
             <: nil
    push $ notification @'TextDocumentDidChangeK param

