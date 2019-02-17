
{-# OPTIONS_GHC -Wall #-}

module LSP.MessageBuilder where

import           RIO
import           Data.Singletons
import           LSP.Record
import           LSP.Method
import           LSP.Types

-------------------------------------------------------------------------------
-- General
-------------------------------------------------------------------------------

-- | Build 'ClientRequest'
request :: forall (m :: ClientRequestMethodK). ImplRequest m
        => ID
        -> RequestParam m
        -> ClientRequest m
request id' a = Request $ Record
                        $ #jsonrpc @= "2.0"
                       <! #id      @= id'
                       <! #method  @= fromSing (sing :: Sing m)
                       <! #params  @= a
                       <! nil

-- | Build 'ClientResponse'
response :: forall (m :: ServerRequestMethodK). SingI m
         => Nullable ID
         -> Option (ResponseResultParam m)
         -> Option (ResponseError (ResponseErrorParam m))
         -> ClientResponse m
response id' resp err = Response $ Record
                                 $ #jsonrpc @= "2.0"
                                <! #id      @= id'
                                <! #result  @= resp
                                <! #error   @= err
                                <! nil

notification :: forall (m :: ClientNotificationMethodK). ImplNotification m
             => NotificationParam m -> Notification m
notification a = Notification $ Record
                              $ #jsonrpc @= "2.0"
                             <! #method  @= fromSing (sing :: Sing m)
                             <! #params  @= a
                             <! nil

-------------------------------------------------------------------------------
-- Initialize
-------------------------------------------------------------------------------

initializeParam :: Nullable Number -> Nullable Uri -> RequestParam 'InitializeK
initializeParam processId rootUri
     = Record
     $ #processId             @= processId
    <! #rootPath              @= None
    <! #rootUri               @= rootUri
    <! #initializationOptions @= None
    <! #capabilities          @=  Record {
                                    fields =
                                       #workspace    @= Some workspaceOption
                                    <! #textDocument @= Some textDocumentOption
                                    <! #experimental @= None
                                    <! nil }
    <! #trace                 @= Some #off
    <! nil
  where
    workspaceOption :: WorkspaceClientCapabilities
    workspaceOption -- {{{
      =  Record
      $  #applyEdit @= Some True
      <! #workspaceEdit @= Some Record {
              fields = #documentChanges @= Some False <! nil
            }
      <! #didChangeConfiguration @= Some Record {
              fields = #dynamicRegistration @= Some False <! nil
            }
      <! #didChangeWatchedFiles @= Some Record {
              fields = #dynamicRegistration @= Some False <! nil
            }
      <! #symbol @= Some Record {
              fields = #dynamicRegistration @= Some False
                    <! #symbolKind @= None -- TODO
                    <! nil
            }
      <! #executeCommand @= noDyn
      <! #workspaceFolders @= Some False
      <! #configuration @= Some False
      <! nil @(Field Identity)
    -- }}}
    textDocumentOption :: TextDocumentClientCapabilities
    textDocumentOption -- {{{
      = Record
      $ #synchronization @= Some Record { fields =
               #dynamicRegistration @= Some False
            <! #willSave @= Some False
            <! #willSaveUntil @= Some False
            <! #didSave @= Some True
            <! nil }
      <! #completion @= Some Record { fields =
               #dynamicRegistration @= Some False
            <! #completionItem @= Some Record { fields =
                     #snippetSupport @= Some True -- use neosnippet
                  <! #commitCharactersSupport @= Some True
                  <! #documentationFormat @= Some [ #plaintext ]
                  <! nil }
            <! #completionItemKind @= Some Record { fields =
                     #valueSet @= None <! nil }
            <! #contextSupport @= Some True -- TODO
            <! nil }
      <! #hover @= Some Record { fields =
               #dynamicRegistration @= Some False
            <! #contentFormat @= Some [ #plaintext ]
            <! nil }
      <! #signatureHelp @= Some Record { fields =
               #dynamicRegistration @= Some False
            <! #signatureInformation @= Some Record { fields =
                     #documentationFormat @= Some [ #plaintext ] <! nil }
            <! nil }
      <! #references @= noDyn
      <! #documentHightlight @= noDyn
      <! #documentSymbol @= Some Record { fields =
               #dynamicRegistration @= Some False
            <! #symbolKind @= None
            <! nil }
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
    noDyn = Some Record { fields = #dynamicRegistration @= Some False <! nil }

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
didOpenTextDocumentParam textDocument = Record $ #textDocument @= textDocument <! nil

