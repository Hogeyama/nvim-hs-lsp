
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wall #-}

-- BuildMessageとかのほうがいいかしら
module Neovim.LSP.Protocol.Messages where

import           Data.Extensible          hiding (Nullable)
import           Data.Singletons

import           Neovim.LSP.Protocol.Type

-------------------------------------------------------------------------------
-- General
-------------------------------------------------------------------------------

notification :: forall (m :: ClientNotificationMethodK). ImplNotification m
             => NotificationParam m -> Notification m
notification a = Notification $ #jsonrpc @= "2.0"
                             <! #method  @= fromSing (sing :: Sing m)
                             <! #params  @= a
                             <! nil

-- | Build 'ClientRequest'
request :: forall (m :: ClientRequestMethodK). ImplRequest m
        => ID
        -> RequestParam m
        -> ClientRequest m
request id' a = Request $ #jsonrpc @= "2.0"
                       <! #id      @= id'
                       <! #method  @= fromSing (sing :: Sing m)
                       <! #params  @= a
                       <! nil

-- | Build 'ClientResponse'
response :: forall (m :: ServerRequestMethodK). SingI m
         => Nullable ID
         -> Option (ResResult m)
         -> Option (ResponseError (ResError m))
         -> ClientResponse m
response id' resp err = Response $ #jsonrpc @= "2.0"
                                <! #id      @= id'
                                <! #result  @= resp
                                <! #error   @= err
                                <! nil

-------------------------------------------------------------------------------
-- Initialize
-------------------------------------------------------------------------------

initializeParam :: Nullable Number -> Nullable Uri -> RequestParam 'InitializeK
initializeParam processId rootUri
     = #processId             @= processId
    <! #rootPath              @= None
    <! #rootUri               @= rootUri
    <! #initializationOptions @= None
    <! #capabilities          @=  ( #workspace    @= Some workspaceOption
                                 <! #textDocument @= Some textDocumentOption
                                 <! #experimental @= None
                                 <! nil )
    <! #trace                 @= Some TraceOff
    <! nil
  where
    workspaceOption :: WorkspaceClientCapabilities
    workspaceOption -- {{{
      =  #applyEdit @= Some True
      <! #workspaceEdit @= Some
            (#documentChanges @= Some False <! nil)
      <! #didChangeConfiguration @= Some
            (#dynamicRegistration @= Some False <! nil)
      <! #didChangeWatchedFiles @= Some
            (#dynamicRegistration @= Some False <! nil)
      <! #symbol @= Some
             ( #dynamicRegistration @= Some False
            <! #symbolKind @= None -- TODO
            <! nil )
      <! #executeCommand @= noDyn
      <! #workspaceFolders @= Some False
      <! #configuration @= Some False
      <! nil
    -- }}}
    textDocumentOption :: TextDocumentClientCapabilities
    textDocumentOption -- {{{
      =  #synchronization @= Some
             ( #dynamicRegistration @= Some False
            <! #willSave @= Some False
            <! #willSaveUntil @= Some False
            <! #didSave @= Some True
            <! nil )
      <! #completion @= Some
             ( #dynamicRegistration @= Some False
            <! #completionItem @= Some
                   ( #snippetSupport @= Some True -- use neosnippet
                  <! #commitCharactersSupport @= Some True
                  <! #documentationFormat @= Some [PlainText]
                  <! nil )
            <! #completionItemKind @= Some
                   ( #valueSet @= None <! nil )
            <! #contextSupport @= Some True -- TODO
            <! nil )
      <! #hover @= Some
             ( #dynamicRegistration @= Some False
            <! #contentFormat @= Some [PlainText]
            <! nil )
      <! #signatureHelp @= Some
             ( #dynamicRegistration @= Some False
            <! #signatureInformation @= Some
                   ( #documentationFormat @= Some [PlainText] <! nil )
            <! nil )
      <! #references @= noDyn
      <! #documentHightlight @= noDyn
      <! #documentSymbol @= Some
             ( #dynamicRegistration @= Some False
            <! #symbolKind @= None
            <! nil )
      <! #formatting         @= noDyn
      <! #rangeFormatting    @= noDyn
      <! #onTypeFormatting   @= noDyn
      <! #definition         @= noDyn
      <! #typeDefinition     @= noDyn
      <! #implementation     @= noDyn
      <! #codeAction         @= noDyn
      <! #codeLens           @= noDyn
      <! #documentLink       @= noDyn
      <! #colorProvider      @= noDyn
      <! #rename             @= noDyn
      <! nil
    -- }}}
    noDyn = Some (#dynamicRegistration @= Some False <! nil)


-------------------------------------------------------------------------------
-- Exit
-------------------------------------------------------------------------------

exitNotification :: Notification 'ExitK
exitNotification = notification exitParam

exitParam :: NotificationParam 'ExitK
exitParam = None

-------------------------------------------------------------------------------
-- DidOpenTextDocument Notification
-------------------------------------------------------------------------------

didOpenTextDocumentParam :: TextDocumentItem
                         -> NotificationParam 'TextDocumentDidOpenK
didOpenTextDocumentParam textDocument = #textDocument @= textDocument <! nil

