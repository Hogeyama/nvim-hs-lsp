
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module LSP.Types
  ( Message
  -- Request
  , Request(..)
  , request
  , RequestMessage
  , RequestParam
  , ImplRequest
  , ClientRequest
  , ServerRequest
  -- Response
  , Response(..)
  , response
  , ResponseMessage
  , ResponseError
  , ResponseResultParam
  , ResponseErrorParam
  , ImplResponse
  , ClientResponse
  , ServerResponse
  -- Notification
  , Notification(..)
  , notification
  , ClientNotification
  , ServerNotification
  , ImplNotification
  , NotificationMessage
  , NotificationParam

  -- Other types
  , ID(..)
  , Number
  , Version
  , Uri(..)
  , pathToUri
  , uriToFilePath
  , uriToAbsFilePath
  , uriToAbsDirPath
  , ErrorCode
  , prettyResponceError
  , TextDocumentSync
  , TextDocumentSyncKind
  , TextDocumentSyncOptions
  , Position
  , Range
  , Location
  , Diagnostic
  , DiagnosticSeverity
  , DiagnosticRelatedInformation
  , Command
  , TextEdit
  , TextDocumentEdit
  , WorkspaceEdit
  , TextDocumentIdentifier
  , textDocumentIdentifier
  , VersionedTextDocumentIdentifier
  , versionedTextDocmentIdentifier
  , TextDocumentItem
  , TextDocumentPositionParams
  , CompletionItem
  , CompletionList
  , DocumentFilter
  , DocumentSelector
  , Trace
  , MessageType
  , MessageActionItem
  , Hover
  , MarkedString
  , MarkupKind
  , MarkupContent
  , ApplyWorkspaceEditResponse
  , WorkspaceClientCapabilities
  , TextDocumentClientCapabilities
  , FormattingOptions(..)
  , SymbolInformation
  , DocumentSymbol(..)
  , CodeAction
  , CodeActionKind
  , LocationLink

  -- Registration Options
  , DocumentOnTypeFormattingRegistrationOptions
  , CodeActionRegistrationOptions
  , RenameRegistrationOptions
  , DidChangeWatchedFilesRegistrationOptions

  -- param
  -- TODO: 拡充
  , initializeParam
  , exitParam
  , didOpenTextDocumentParam
  )
  where

import           Prelude                         (Enum (toEnum))
import           RIO                             hiding (Void)
import           RIO.Char                        (toLower)
import           RIO.Char.Partial                (digitToInt)
import           RIO.List.Partial                (tail)
import qualified RIO.Text                        as T

import           Data.Aeson                      hiding (Error)
import           Data.Extensible                 as E hiding (Nullable, Record,
                                                       record)
import           Data.Singletons                 (SingI, SingKind(..), sing)
import           Data.TypeLits
import           Path                            (Path, Abs, File, Dir,
                                                  parseAbsFile, parseAbsDir,
                                                  toFilePath)

import           Neovim                          (NvimObject(..))
import           Data.Kind                       (Constraint)
import           LSP.Method
import           LSP.Record
import           LSP.Enum

-- Interfaces defined in
-- https://microsoft.github.io/language-server-protocol/specification

-------------------------------------------------------------------------------
-- Base Protocol -- {{{
-------------------------------------------------------------------------------

-- Message
----------
type Message = Record MessageF
type MessageF = '[ "jsonrpc" >: String ]

-- RequestMessage
-----------------
newtype Request (m :: k) =
  Request (RequestMessage (Demote k) (RequestParam m))
  deriving Generic
type family RequestParam (m :: k)

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

type RequestMessage  m a = Record (RequestMessageF m a)
type RequestMessageF m a = MessageF ++
  '[ "id"      >: ID
   , "method"  >: m
   , "params"  >: a
   ]

type ImplRequest (m :: k) =
  ( SingI m
  , Typeable m
  , IsMethodKind k
  , Eq        (RequestParam m)
  , Show      (RequestParam m)
  , FieldJSON (RequestParam m)
  )
deriving instance ImplRequest m => Eq       (Request m)
deriving instance ImplRequest m => Show     (Request m)
deriving instance ImplRequest m => ToJSON   (Request m)
deriving instance ImplRequest m => FromJSON (Request m)

type ClientRequest (m :: ClientRequestMethodK) = Request m
type ServerRequest (m :: ServerRequestMethodK) = Request m

-- Response Message
------------------
newtype Response (m :: k) =
  Response (ResponseMessage (ResponseResultParam m) (ResponseErrorParam m))
  deriving Generic
type family ResponseResultParam (m :: k)
type family ResponseErrorParam  (m :: k)

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

type ResponseMessage  a e = Record (ResponseMessageF a e)
type ResponseMessageF a e = MessageF ++
  '[ "id"      >: Nullable ID
   , "result"  >: Option a
   , "error"   >: Option (ResponseError e)
   ]
type ResponseError  e = Record (ResponseErrorF e)
type ResponseErrorF e =
  '[ "code"    >: ErrorCode -- TODO :|: Int
   , "message" >: Text
   , "data"    >: Option e
   ]
type ErrorCode = EnumN
 '[ "parseError"           >: 'Neg 32700
  , "invalidRequest"       >: 'Neg 32600
  , "methodNotFound"       >: 'Neg 32601
  , "invalidParams"        >: 'Neg 32602
  , "internalError"        >: 'Neg 32603
  , "serverErrorStart"     >: 'Neg 32099
  , "serverErrorEnd"       >: 'Neg 32000
  , "serverNotInitialized" >: 'Neg 32002
  , "unknownErrorCode"     >: 'Neg 32001
  , "requestCancelled"     >: 'Neg 32800
  ]

prettyResponceError :: Show e => ResponseError e -> Text
prettyResponceError err =
    tshow (err^. #code) <> ": " <> err^. #message <> mdata
  where
    mdata = case err^. #data of
      None   -> mempty
      Some d -> ": " <> tshow d

type ImplResponse (m :: k) =
  ( SingI m
  , Typeable m
  , IsMethodKind k
  , Eq        (ResponseResultParam m)
  , Show      (ResponseResultParam m)
  , ToJSON    (ResponseResultParam m) -- TODO: ResultはOptionで包むのでFieldJSONではダメ
  , FromJSON  (ResponseResultParam m) --       綺麗に書けないかなあ
  , Eq        (ResponseErrorParam  m)
  , Show      (ResponseErrorParam  m)
  , ToJSON    (ResponseErrorParam  m)
  , FromJSON  (ResponseErrorParam  m)
  )
deriving instance ImplResponse m => Eq       (Response m)
deriving instance ImplResponse m => Show     (Response m)
deriving instance ImplResponse m => ToJSON   (Response m)
deriving instance ImplResponse m => FromJSON (Response m)

type ClientResponse (m :: ServerRequestMethodK) = Response m
type ServerResponse (m :: ClientRequestMethodK) = Response m

-- Notification Message
-----------------------
newtype Notification (m :: k) =
  Notification (NotificationMessage (Demote k) (NotificationParam m))
  deriving Generic
type family NotificationParam (m :: k)

notification :: forall (m :: ClientNotificationMethodK). ImplNotification m
             => NotificationParam m -> Notification m
notification a = Notification $ Record
                              $ #jsonrpc @= "2.0"
                             <! #method  @= fromSing (sing :: Sing m)
                             <! #params  @= a
                             <! nil

type NotificationMessage  m a = Record (NotificationMessageF m a)
type NotificationMessageF m a = MessageF ++
  '[ "method"  >: m
   , "params"  >: a -- NOTE 本来はOption a
   ]

type ImplNotification (m :: k) =
  ( SingI m
  , Typeable m
  , IsMethodKind k
  , Eq        (NotificationParam m)
  , Show      (NotificationParam m)
  , FieldJSON (NotificationParam m)
  )
deriving instance ImplNotification m => Eq       (Notification m)
deriving instance ImplNotification m => Show     (Notification m)
deriving instance ImplNotification m => ToJSON   (Notification m)
deriving instance ImplNotification m => FromJSON (Notification m)

type ClientNotification (m :: ClientNotificationMethodK) = Notification m
type ServerNotification (m :: ServerNotificationMethodK) = Notification m

--}}}

-------------------------------------------------------------------------------
-- Common Data --{{{
-------------------------------------------------------------------------------

-- Number
-------------------------------------------------------------------------------
type Number = Double
type Version = Int

-- ID
----------------------------------------
data ID = IDNum Number | IDString String
  deriving (Show, Eq, Ord, Generic)
instance FromJSON ID where
  parseJSON (Number n) = return $ IDNum $ realToFrac n
  parseJSON (String s) = return $ IDString $ T.unpack s
  parseJSON _          = mempty
instance ToJSON ID where
  toJSON (IDNum n)    = toJSON n
  toJSON (IDString s) = toJSON s
instance Hashable ID

-- Uri
----------------------------------------

newtype Uri = Uri { getUri :: Text }
  deriving (Eq, Ord, Read, Show, Generic, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

uriToFilePath :: Uri -> FilePath
uriToFilePath (Uri uri)
  | "file://" `T.isPrefixOf` uri = platformAdjust . uriDecode . T.unpack $ T.drop n uri
  | otherwise = error $ "uriToFilePath: invalid input: " ++ show uri
      where
        n = T.length "file://"

        uriDecode ('%':x:y:rest) = toEnum (16 * digitToInt x + digitToInt y) : uriDecode rest
        uriDecode (x:xs) = x : uriDecode xs
        uriDecode [] = []

        -- Drop leading '/' for absolute Windows paths
        platformAdjust path@('/':_drive:':':_rest) = tail path
        platformAdjust path                        = path

uriToAbsFilePath :: Uri -> Path Abs File
uriToAbsFilePath uri =
    fromMaybe (error "impossible") (parseAbsFile (uriToFilePath uri))

uriToAbsDirPath :: Uri -> Path Abs Dir
uriToAbsDirPath uri =
    fromMaybe (error "impossible") (parseAbsDir (uriToFilePath uri))

pathToUri :: Path Abs b -> Uri
pathToUri (toFilePath -> (drive:':':rest)) = Uri $
    T.pack $ concat ["file:///", [toLower drive], "%3A", fmap convertDelim rest]
  where
    convertDelim '\\' = '/'
    convertDelim c    = c
pathToUri file = Uri $ T.pack $ "file://" ++ toFilePath file

-- Position
----------------------------------------
type Position = Record
  '[ "line"      >: Int
   , "character" >: Int
   ]

-- Range
----------------------------------------
type Range = Record
  '[ "start" >: Position
   , "end"   >: Position
   ]

-- Location
----------------------------------------

-- """
-- Represents a location inside a resource, such as a line inside a text file.
-- """
type Location = Record
  '[ "uri"   >: Uri
   , "range" >: Range
   ]

-- """
-- Represents a link between a source and a target location.
-- """
type LocationLink = Record
 '[ "originSelectionRange" >: Option Range
  , "targetUri" >: Uri
  , "targetRange" >: Range
  , "targetSelectionRange" >: Range
  ]

-- Diagnostic
----------------------------------------
type Diagnostic = Record
  '[ "range"              >: Range
   , "severity"           >: Option DiagnosticSeverity
   , "code"               >: Option (ErrorCode :|: String)
   , "source"             >: Option String
   , "message"            >: Text
   , "relatedInformation" >: Option [DiagnosticRelatedInformation]
   ]

-- |
-- >>> :m + Data.List
-- >>> sort ([#error, #warning, #hint] :: [DiagnosticSeverity])
-- [#error,#warning,#hint]
type DiagnosticSeverity = EnumN
  '[ "error"       >: 'Pos 1
   , "warning"     >: 'Pos 2
   , "information" >: 'Pos 3
   , "hint"        >: 'Pos 4
   ]

-- """
-- Represents a related message and source code location for a diagnostic. This should be
-- used to point to code locations that cause or related to a diagnostics, e.g when duplicating
-- a symbol in a scope.
-- """
type DiagnosticRelatedInformation = Record
 '[ "location" >: Location
  , "message"  >: String
  ]
-- Command
----------------------------------------
type Command = Record
  '[ "title"     >: String
   , "command"   >: String
   , "arguments" >: Option [Value]
   ]

-- TextEdit / TextDocumentEdit
----------------------------------------
type TextEdit = Record
  '[ "range"   >: Range
   , "newText" >: Text
   ]
type TextDocumentEdit = Record
  '[ "textDocument" >: VersionedTextDocumentIdentifier
   , "edits"        >: [TextEdit]
   ]

-- File Resource changes
----------------------------------------
type CreateFile = Record
  '[ "kind"    >: ResourceOperationKind
   , "uri"     >: Uri
   , "options" >: Option CreateFileOptions
   ]
type CreateFileOptions = Record
  '[ "overwrite"      >: Bool
   , "ignoreIfExists" >: Bool
   ]
type RenameFile = Record
  '[ "overwrite"      >: Bool
   , "oldUri"         >: Uri
   , "newUri"         >: Uri
   , "options"        >: Option RenameFileOptions
   ]
type RenameFileOptions = Record
  '[ "overwrite"      >: Bool
   , "ignoreIfExists" >: Bool
   ]
type DeleteFile = Record
  '[ "kind"    >: ResourceOperationKind
   , "uri"     >: Uri
   , "options" >: Option DeleteFileOptions
   ]
type DeleteFileOptions = Record
  '[ "recursive" >: Bool
   , "ignoreIfExists" >: Bool
   ]
type ResourceOperationKind = EnumS
  '[ "create"
   , "rename"
   , "delete"
   ]

-- WorkspaceEdit
----------------------------------------
type WorkspaceEdit = Record
  '[ "changes"         >: Option (Map Uri [TextEdit])
   , "documentChanges" >: Option ([TextDocumentEdit]
                              :|: [  TextDocumentEdit
                                 :|: CreateFile
                                 :|: RenameFile
                                 :|: DeleteFile
                                  ])
   ]

-- TextDocumentIdentifier
----------------------------------------
type TextDocumentIdentifier  = Record TextDocumentIdentifierF
type TextDocumentIdentifierF = '[ "uri" >: Uri ]

textDocumentIdentifier :: Uri -> TextDocumentIdentifier
textDocumentIdentifier uri = Record $ uri =<: nil

-- VersionedTextDocumentIdentifier
----------------------------------------
type VersionedTextDocumentIdentifier  = Record VersionedTextDocumentIdentifierF
type VersionedTextDocumentIdentifierF =
  TextDocumentIdentifierF ++
  '[ "version" >: Nullable Version
   ]

versionedTextDocmentIdentifier :: Uri -> Version -> VersionedTextDocumentIdentifier
versionedTextDocmentIdentifier uri version = Record (uri =<: Just version =<: nil)

-- TextDocumentItem
--   An item to transfer a text document from the client to the server.
----------------------------------------
type TextDocumentItem = Record
  '[ "uri"        >: Uri
   , "languageId" >: String
   , "version"    >: Version -- undo/redoを含め，編集のたびに増える
   , "text"       >: Text
   ]

-- TextDocumentPositionParams
----------------------------------------
type TextDocumentPositionParams  = Record TextDocumentPositionParamsF
type TextDocumentPositionParamsF =
  '[ "textDocument" >: TextDocumentIdentifier
   , "position"     >: Position
   ]

-- DocumentFilter
----------------------------------------
type DocumentFilter = Record
  '[ "language" >: Option String
   , "scheme"   >: Option String
   , "pattern"  >: Option String
   ]
type DocumentSelector = [DocumentFilter]

-- MarkupContent
----------------------------------------
type MarkupKind = EnumS '[ "plaintext", "markdown" ]

type MarkupContent = Record
  '[ "kind"  >: MarkupKind
   , "value" >: String
   ]

--}}}

-------------------------------------------------------------------------------
-- Client Request --{{{
-------------------------------------------------------------------------------

-- Initialize {{{
----------------------------------------

-- | Initialize Request
type instance RequestParam 'InitializeK = Record
  '[ "processId"             >: Nullable Number
   , "rootPath"              >: Option (Nullable String)
   , "rootUri"               >: Nullable Uri  -- rootPathよりこっち推奨
   , "initializationOptions" >: Option Value
   , "capabilities"          >: ClientCapabilities
   , "trace"                 >: Option Trace
   , "workspaceFolders"      >: Option (Nullable [WorkspaceFolder])
   ]

-- | Initialize Response > Result
type instance ResponseResultParam 'InitializeK = Record
  '[ "capabilities" >: ServerCapabilities
   ]

-- """
-- * Indicates whether the client execute the following retry logic:
-- * (1) show the message provided by the ResponseError to the user
-- * (2) user selects retry or cancel
-- * (3) if user selected retry the initialize method is sent again.
-- """
-- | Initialize Response > Error
type instance ResponseErrorParam 'InitializeK = Record
  '[ "retry" >: Bool
   ]

initializeParam :: Nullable Number -> Nullable Uri -> RequestParam 'InitializeK -- {{{
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
    <! #workspaceFolders      @= None -- TODO
    <! nil
  where
    workspaceOption :: WorkspaceClientCapabilities
    workspaceOption -- {{{
      =  Record
      $  #applyEdit @= Some True
      <! #workspaceEdit @= Some Record {
              fields = #documentChanges   @= Some False
                    <! #resouceOperations @= None -- TODO
                    <! #failureHandling   @= None -- TODO
                    <! nil
            }
      <! #didChangeConfiguration @= Some Record {
              fields = #dynamicRegistration @= Some True <! nil
            }
      <! #didChangeWatchedFiles @= Some Record {
              fields = #dynamicRegistration @= Some False <! nil
            }
      <! #symbol @= Some Record {
              fields = #dynamicRegistration @= Some False
                    <! #symbolKind @= None -- TODO
                    <! nil
            }
      <! #executeCommand @= Some noDyn
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
                     #snippetSupport @= Some False
                  <! #commitCharactersSupport @= Some True
                  <! #documentationFormat @= Some [ #plaintext ]
                  <! #deprecatedSupport   @= None -- TODO
                  <! #preselectSupport    @= None
                  <! nil }
            <! #completionItemKind @= Some Record { fields =
                     #valueSet @= None <! nil }
            <! #contextSupport @= Some False -- TODO
            <! nil }
      <! #hover @= Some Record { fields =
               #dynamicRegistration @= Some False
            <! #contentFormat @= Some [ #plaintext ]
            <! nil }
      <! #signatureHelp @= Some Record { fields =
               #dynamicRegistration @= Some False
            <! #signatureInformation @= Some Record { fields =
                     #documentationFormat  @= Some [ #plaintext ]
                  <! #parameterInformation @=  None
                  <! nil }
            <! nil }
      <! #references @= Some noDyn
      <! #documentHightlight @= Some noDyn
      <! #documentSymbol @= Some Record { fields =
               #dynamicRegistration @= Some False
            <! #symbolKind @= None
            <! #hierarchicalDocumentSymbolSupport @= None
            <! nil }
      <! #formatting         @= Some noDyn
      <! #rangeFormatting    @= Some noDyn
      <! #onTypeFormatting   @= Some noDyn
      <! #declaration        @= Some noDynNoLink
      <! #definition         @= Some noDynNoLink
      <! #typeDefinition     @= None -- TODO
      <! #implementation     @= None -- TODO
      <! #codeAction         @= None -- TODO
      <! #codeLens           @= Some noDyn
      <! #documentLink       @= Some noDyn
      <! #colorProvider      @= Some noDyn
      <! #rename             @= Some noDyn
      <! #publishDiagnostics @= None -- TODO
      <! #foldingRange       @= None -- TODO
      <! nil
    -- }}}
    noDyn = Record { fields = #dynamicRegistration @= Some False <! nil }
    noDynNoLink = Record { fields =
                  #dynamicRegistration @= Some False
               <! #linkSupport         @= Some False
               <! nil }
-- }}}

-- Other Data
--------------

type Trace = EnumS '[ "off", "messages", "verbose" ]

type FailureHandlingKind = EnumS
  '[ "abort"
   , "transactional"
   , "undo"
   , "textOnlyTransaction"
   ]

type ClientCapabilities = Record
  '[ "workspace"    >: Option WorkspaceClientCapabilities
   , "textDocument" >: Option TextDocumentClientCapabilities
   , "experimental" >: Option Value
   ]

type OptionalRecord xs = Option (Record xs)

type WorkspaceClientCapabilities = Record
  -- {{{
  '[ "applyEdit" >: Option Bool
   , "workspaceEdit" >: OptionalRecord
        '[ "documentChanges"   >: Option Bool
         , "resouceOperations" >: Option [ResourceOperationKind]
         , "failureHandling"   >: Option FailureHandlingKind
         ]
   , "didChangeConfiguration" >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "didChangeWatchedFiles" >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "symbol" >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         , "symbolKind" >: OptionalRecord
              '[ "valueSet" >: Option [SymbolKind]
               ]
         ]
   , "executeCommand" >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "workspaceFolders" >: Option Bool
   , "configuration" >: Option Bool
   ]
  -- }}}

type TextDocumentClientCapabilities = Record
  -- {{{
  '[ "synchronization"    >: OptionalRecord
        '[ "dynamicRegistration"  >: Option Bool
         , "willSave"             >: Option Bool
         , "willSaveUntil"        >: Option Bool
         , "didSave"              >: Option Bool
         ]
   , "completion"         >: OptionalRecord
        '[ "dynamicRegistration"  >: Option Bool
         , "completionItem"       >: OptionalRecord
              '[ "snippetSupport"          >: Option Bool
               , "commitCharactersSupport" >: Option Bool
               , "documentationFormat"     >: Option [MarkupKind]
               , "deprecatedSupport"       >: Option Bool
               , "preselectSupport"        >: Option Bool
               ]
         , "completionItemKind"   >: OptionalRecord
              '[ "valueSet" >: Option [CompletionItemKind]
               ]
         , "contextSupport"       >: Option Bool
         ]
   , "hover"              >: OptionalRecord
        '[ "dynamicRegistration"  >: Option Bool
         , "contentFormat"        >: Option [MarkupKind]
         ]
   , "signatureHelp"      >: OptionalRecord
        '[ "dynamicRegistration"  >: Option Bool
         , "signatureInformation" >: OptionalRecord
              '[ "documentationFormat" >: Option [MarkupKind]
               , "parameterInformation" >: OptionalRecord
                    '[ "labelOffsetSupport" >: Option Bool
                     ]
               ]
         ]
   , "references"         >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "documentHightlight" >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "documentSymbol"     >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         , "symbolKind"          >: OptionalRecord
              '[ "valueSet" >: Option [SymbolKind]
               ]
         , "hierarchicalDocumentSymbolSupport" >: Option Bool -- TODO
         ]
   , "formatting"         >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "rangeFormatting"    >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "onTypeFormatting"   >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "declaration"        >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         , "linkSupport"         >: Option Bool
         ]
   , "definition"         >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         , "linkSupport"         >: Option Bool
         ]
   , "typeDefinition"     >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         , "linkSupport"         >: Option Bool
         ]
   , "implementation"     >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         , "linkSupport"         >: Option Bool
         ]
   , "codeAction"         >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         , "codeActionLiteralSupport" >: OptionalRecord
              '[ "codeActionKind" >: Record
                    '[ "valueSet" >: [CodeActionKind]
                     ]
               ]
         ]
   , "codeLens"           >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "documentLink"       >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "colorProvider"      >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "rename"             >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "publishDiagnostics" >: OptionalRecord
        '[ "realatedInformation" >: Option Bool
         ]
   , "foldingRange"       >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         , "rangeLimity"         >: Option Int
         , "lineFoldingOnly"     >: Option Bool
         ]
   ]
-- }}}

type ServerCapabilities = Record
  '[ --"""
     -- Defines how text documents are synced. Is either a detailed structure
     -- defining each notification or for backwards compatibility the
     -- TextDocumentSyncKind number. If omitted it defaults to `TextDocumentSyncKind.None`.
     -- """
     "textDocumentSync"                 >: Option TextDocumentSync
   , "hoverProvider"                    >: Option HoverOptions
   , "completionProvider"               >: Option CompletionOptions
   , "signatureHelpProvider"            >: Option SignatureHelpOptions
   , "definitionProvider"               >: Option DefinitionOptions
   , "typeDefinitionProvider"           >: Option TypeDefinitionOptions
   , "implementationProvider"           >: Option ImplementationOptions
   , "referencesProvider"               >: Option ReferenceOptions
   , "documentHighlightProvider"        >: Option DocumentHighlightOptions
   , "documentSymbolProvider"           >: Option DocumentSymbolOptions
   , "workspaceSymbolProvider"          >: Option WorkspaceSymbolOptions
     -- """
     -- The server provides code actions. The `CodeActionOptions` return type is only
     -- valid if the client signals code action literal support via the property
     -- `textDocument.codeAction.codeActionLiteralSupport`.
     -- """
   , "codeActionProvider"               >: Option (Bool :|: CodeActionOptions)
   , "codeLensProvider"                 >: Option CodeLensOptions
   , "documentFormattingProvider"       >: Option DocumentFormattingOptions
   , "documentRangeFormattingProvider"  >: Option DocumentRangeFormattingOptions
   , "documentOnTypeFormattingProvider" >: Option DocumentOnTypeFormattingOptions
   , "renameProvider"                   >: Option (Bool :|: RenameOptions)
   , "documentLinkProvider"             >: Option DocumentLinkOptions
   , "colorProvider"                    >: Option ColorOptions
   , "foldingRangeProvider"             >: Option FoldingRangeOptions
   , "executeCommandProvider"           >: Option ExecuteCommandOptions
   , "workspace"                        >: Option WorkspaceOptions
   , "experimental"                     >: Option Value
   ]

type TextDocumentSyncKind = EnumN
  '[ "none"        >: 'Pos 0
   , "fulL"        >: 'Pos 1
   , "incremental" >: 'Pos 2
   ]

type HoverOptions = Bool

type CompletionOptions = Record
  '[ "resolveProvider"   >: Option Bool
   , "triggerCharacters" >: Option [String]
   ]

type SignatureHelpOptions = Record
  '[ "triggerCharacters" >: Option [String]
   ]

-- Note: This type alias is not defined in original specification
type DefinitionOptions = Bool

type TypeDefinitionOptions = Bool :|:
  TextDocumentRegistrationOptions & StaticRegistrationOptions

type ImplementationOptions = Bool :|:
  TextDocumentRegistrationOptions & StaticRegistrationOptions

-- Note: This type alias is not defined in original specification
type ReferenceOptions = Bool

-- Note: This type alias is not defined in original specification
type DocumentHighlightOptions = Bool

-- Note: This type alias is not defined in original specification
type DocumentSymbolOptions = Bool

-- Note: This type alias is not defined in original specification
type WorkspaceSymbolOptions = Bool

type CodeActionOptions = Record
  '[ "codeActionKinds" >: Option [CodeActionKind]
   ]

type CodeLensOptions = Record
  '[ "resolveProvider" >: Option Bool
   ]

-- Note: This type alias is not defined in original specification
type DocumentFormattingOptions = Bool

-- Note: This type alias is not defined in original specification
type DocumentRangeFormattingOptions = Bool

type DocumentOnTypeFormattingOptions = Record
  '[ "firstTriggerCharacter" >: String
   , "moreTriggerCharacter"  >: Option [String]
   ]

type RenameOptions = Record
  '[ "prepareProvider" >: Option Bool
   ]

type DocumentLinkOptions = Record
  '[ "resolveProvider" >: Option Bool
   ]

-- Note: This type alias is not defined in original specification
type ColorOptions = Bool :|: ColorProviderOptions :|:
  ColorProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions

-- Note: This type alias is not defined in original specification
type FoldingRangeOptions = Bool :|: FoldingRangeProviderOptions :|:
  FoldingRangeProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions

type ExecuteCommandOptions = Record
  '[ "commands" >: [String]
   ]

type WorkspaceOptions = Record
 '[ "workspaceFolders" >: OptionalRecord
        '[ "supported"           >: Option Bool
         , "changeNotifications" >: (String :|: Bool)
         ]
  ]

type SaveOptions = Record
  '[ "includeText" >: Option Bool
   ]

type ColorProviderOptions = Record '[]

type FoldingRangeProviderOptions = Record '[]

type TextDocumentSyncOptions = Record
  '[ "openClose"         >: Option Bool
   , "change"            >: Option TextDocumentSyncKind
   , "willSave"          >: Option Bool
   , "willSaveWaitUntil" >: Option Bool
   , "save"              >: Option SaveOptions
   ]

type StaticRegistrationOptions = Record
  '[ "id" >: Option String
   ]

type MarkedString = String :|: Record
  '[ "language" >: String
   , "value"    >: String
   ]

-- 後回し
type SymbolKind = Value -- TODO
type CompletionItemKind = Value

type TextDocumentSync = TextDocumentSyncOptions :|: TextDocumentSyncKind
--}}}

-- Shutdown {{{
----------------------------------------
type instance RequestParam 'ShutdownK = Option Void
type instance ResponseResultParam 'ShutdownK = Void
type instance ResponseErrorParam  'ShutdownK = Value
-- }}}

-- WorkspaceSymbol {{{
----------------------------------------
type instance RequestParam 'WorkspaceSymbolK = Record
  '[ "query" >: String ]
type instance ResponseResultParam 'WorkspaceSymbolK = Nullable [SymbolInformation]
type instance ResponseErrorParam  'WorkspaceSymbolK = Value

-- }}}

-- WorkspaceExecuteCommand {{{
----------------------------------------
type instance RequestParam 'WorkspaceExecuteCommandK = ExecuteCommandParams
type instance ResponseResultParam 'WorkspaceExecuteCommandK = Nullable Value
type instance ResponseErrorParam  'WorkspaceExecuteCommandK = String

type ExecuteCommandParams = Record
  '[ "command"   >: String
   , "arguments" >: Option [Value]
   ]

-- }}}

-- TextDocumentWillSaveWaitUntil {{{
----------------------------------------
type instance RequestParam 'TextDocumentWillSaveWaitUntilK = WillSaveTextDocumentParams
type instance ResponseResultParam 'TextDocumentWillSaveWaitUntilK = Void
type instance ResponseErrorParam  'TextDocumentWillSaveWaitUntilK = Value
type WillSaveTextDocumentParams = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "reason"       >: TextDocumentSaveReason
   ]
type TextDocumentSaveReason = EnumN
  '[ "manual"     >: 'Pos 1
   , "afterDelay" >: 'Pos 2
   , "focusOut"   >: 'Pos 3
   ]

-- }}}

-- TextDocumentCompletion {{{
----------------------------------------
type instance RequestParam 'TextDocumentCompletionK = CompletionParams
type instance ResponseResultParam 'TextDocumentCompletionK = Nullable ([CompletionItem] :|: CompletionList)
type instance ResponseErrorParam  'TextDocumentCompletionK = String

type CompletionList = Record
  '[ "isIncomplete" >: Bool
   , "items"        >: [CompletionItem]
   ]
type CompletionItem = Record
  '[ "label"               >: String
   , "kind"                >: Option Int
   , "detail"              >: Option String
   , "documentation"       >: Option (String :|: MarkupContent)
   , "sortText"            >: Option String
   , "filterText"          >: Option String
   , "insertText"          >: Option String
   , "insertTextFormat"    >: Option InsertTextFormat
   , "textEdit"            >: Option TextEdit
   , "additionalTextEdits" >: Option [TextEdit]
   , "commitCharacters"    >: Option [String]
   , "command"             >: Option Command
   , "data"                >: Option Value
   ]

type InsertTextFormat = Value

type CompletionParams  = Record CompletionParamsF
type CompletionParamsF =
  TextDocumentPositionParamsF ++
  '[ "context" >: Option CompletionContext
   ]
type CompletionContext = Value

-- }}}

-- CompletionItemResolve {{{
----------------------------------------
type instance RequestParam 'CompletionItemResolveK = CompletionItem
type instance ResponseResultParam 'CompletionItemResolveK = CompletionItem
type instance ResponseErrorParam  'CompletionItemResolveK = Value

-- }}}

-- TextDocumentHover {{{
----------------------------------------
type instance RequestParam 'TextDocumentHoverK = TextDocumentPositionParams
type instance ResponseResultParam 'TextDocumentHoverK = Nullable Hover
type instance ResponseErrorParam  'TextDocumentHoverK = Value
  --  "error: code and message set in case an exception happens during the hover request."

type Hover = Record
  '[ "contents" >: (MarkedString :|: [MarkedString] :|: MarkupContent)
   , "range"    >: Option Range
   ]

-- }}}

-- TextDocumentSignatureHelp {{{
----------------------------------------
type instance RequestParam 'TextDocumentSignatureHelpK = TextDocumentPositionParams
type instance ResponseResultParam 'TextDocumentSignatureHelpK = SignatureHelp
type instance ResponseErrorParam  'TextDocumentSignatureHelpK = Value

type SignatureHelp = Value
  -- HIEが対応していないので後回しで良い

-- }}}

-- TextDocumentDefinition {{{
----------------------------------------
type instance RequestParam 'TextDocumentDefinitionK = TextDocumentPositionParams
type instance ResponseResultParam 'TextDocumentDefinitionK = Nullable [Location]
type instance ResponseErrorParam  'TextDocumentDefinitionK = String

-- }}}

-- TextDocumentReferences {{{
----------------------------------------
type instance RequestParam 'TextDocumentReferencesK = ReferenceParams
type instance ResponseResultParam 'TextDocumentReferencesK = Nullable [Location]
type instance ResponseErrorParam  'TextDocumentReferencesK = String

type ReferenceParams  = Record ReferenceParamsF
type ReferenceParamsF =
  TextDocumentPositionParamsF ++
  '[ "context" >: ReferenceContext
   ]
type ReferenceContext = Record
  '[ "includeDeclaration" >: Bool
   ]

-- }}}

-- TextDocumentDocumentHighlight {{{
----------------------------------------
type instance RequestParam 'TextDocumentDocumentHighlightK = TextDocumentPositionParams
type instance ResponseResultParam 'TextDocumentDocumentHighlightK = Nullable [DocumentHighlight]
type instance ResponseErrorParam  'TextDocumentDocumentHighlightK = Value
type DocumentHighlight = Record
  '[ "range" >: Range
   , "kind"  >: Option DocumentHighlightKind
   ]
type DocumentHighlightKind = EnumN
  '[ "text"  >: 'Pos 1
   , "read"  >: 'Pos 2
   , "write" >: 'Pos 3
   ]

-- }}}

-- TextDocumentDocumentSymbol {{{
----------------------------------------
type instance RequestParam 'TextDocumentDocumentSymbolK = Record
  '[ "textDocument" >: TextDocumentIdentifier ]
type instance ResponseResultParam 'TextDocumentDocumentSymbolK = Nullable ([DocumentSymbol] :|: [SymbolInformation])
type instance ResponseErrorParam  'TextDocumentDocumentSymbolK = Value

newtype DocumentSymbol = DocumentSymbol (Record
  '[ "name"       >: String
   , "detail"     >: Option String
   , "kind"       >: SymbolKind
   , "deprecated" >: Option Bool
   , "range"      >: Range
   , "children"   >: Option [DocumentSymbol]
   ])
  deriving (Show,Eq,ToJSON,FromJSON)

type SymbolInformation = Record
  '[ "name"          >: String
   , "kind"          >: SymbolKind
   , "deprecated"    >: Option Bool
   , "location"      >: Location
   , "containerName" >: Option String
   ]

--}}}

-- TextDocumentFormatting {{{
----------------------------------------
type instance RequestParam 'TextDocumentFormattingK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "options"      >: FormattingOptions
   ]
type instance ResponseResultParam 'TextDocumentFormattingK = Nullable [TextEdit]
type instance ResponseErrorParam  'TextDocumentFormattingK = String

-- FormattingOptions {{{
-- |
-- Original definition is:
--
-- @
-- interface FormattingOptions {
--   tabSize: number;
--   insertSpaces: boolean;
--   [key: string]: boolean | number | string;
-- }
-- @
--
-- Usage:
--
-- >>> FormattingOptions $ Record $ #tabSize @= 0 <! #insertSpaces @= False <! #foo @= ("bar" :: String) <! nil
-- FormattingOptions Record {fields = tabSize @= 0.0 <: insertSpaces @= False <: foo @= "bar" <: nil}
--
data FormattingOptions where
  FormattingOptions
    :: forall xs.
          ( Associate "tabSize" Number xs
          , Associate "insertSpaces" Bool xs
          , Forall (E.KeyValue KnownSymbol FOField) xs
          , Eq (Record xs)
          , Show (Record xs)
          , ToJSON (Record xs)
          , FromJSON (Record xs)
          , NvimObject (Record xs)
          , NFData (Record xs)
          )
    => Record xs -> FormattingOptions
instance Show FormattingOptions where
  show (FormattingOptions x) = "FormattingOptions " <> show x
instance Eq FormattingOptions where
  FormattingOptions x == FormattingOptions y = toJSON x == toJSON y
instance ToJSON FormattingOptions where
  toJSON (FormattingOptions xs) = toJSON xs
instance FromJSON FormattingOptions where
  parseJSON x = -- tenuki
    FormattingOptions
      @'["tabSize" >: Number, "insertSpaces" >: Bool]
      <$> parseJSON x
instance NFData FormattingOptions where
  rnf (FormattingOptions xs) = rnf xs
instance NvimObject FormattingOptions where
  fromObject o = FormattingOptions <$>
    fromObject @(Record '["tabSize" >: Number, "insertSpaces" >: Bool]) o
  toObject (FormattingOptions fo) = toObject fo
type family FOField' x :: Constraint where
  FOField' Number = ()
  FOField' Bool = ()
  FOField' String = ()
  FOField' x = TypeError ('Text "Unexpected Type for FOField: " ':<>: 'ShowType x)
class FOField' x => FOField x
instance FOField' x => FOField x
-- }}}

-- }}}

-- TextDocumentRangeFormatting {{{
----------------------------------------
type instance RequestParam 'TextDocumentRangeFormattingK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "range"        >: Range
   , "options"      >: FormattingOptions
   ]
type instance ResponseResultParam 'TextDocumentRangeFormattingK = Nullable [TextEdit]
type instance ResponseErrorParam  'TextDocumentRangeFormattingK = String

--}}}

-- TextDocumentOnTypeFormatting {{{
----------------------------------------
type instance RequestParam 'TextDocumentOnTypeFormattingK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "position"     >: Position
   , "ch"           >: String
   , "options"      >: FormattingOptions
   ]
type instance ResponseResultParam 'TextDocumentOnTypeFormattingK = Nullable [TextEdit]
type instance ResponseErrorParam  'TextDocumentOnTypeFormattingK = Value

type DocumentOnTypeFormattingRegistrationOptions = TextDocumentRegistrationOptions & Record
  '[ "firstTriggerCharacter" >: String
   , "moreTriggerCharacter"  >: Option [String]
   ]
--}}}

-- TextDocumentCodeAction {{{
----------------------------------------

type instance RequestParam 'TextDocumentCodeActionK = CodeActionParams
type instance ResponseResultParam 'TextDocumentCodeActionK = Nullable [Command :|: CodeAction]
type instance ResponseErrorParam  'TextDocumentCodeActionK = Value

type CodeActionParams = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "range"        >: Range
   , "context"      >: CodeActionContext
   ]
type CodeActionContext = Record
  '[ "diagnostics" >: [Diagnostic]
   , "only"        >: Option [CodeActionKind]
   ]

type CodeActionKind = EnumS
  '[ "quickfix"
   , "refactor"
   , "refactor.extract"
   , "refactor.inline"
   , "refactor.rewrite"
   , "source"
   ]

type CodeAction = Record
  '[ "title"       >: String
   , "kind"        >: Option CodeActionKind
   , "diagnostics" >: Option [Diagnostic]
   , "edit"        >: Option WorkspaceEdit
   , "command"     >: Command
   ]

-- for dynamic registration
type CodeActionRegistrationOptions =
  TextDocumentRegistrationOptions & CodeActionOptions

-- }}}

-- TextDocumentCodeLens {{{
----------------------------------------

type instance RequestParam 'TextDocumentCodeLensK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   ]
type instance ResponseResultParam 'TextDocumentCodeLensK = Nullable [CodeLens]
type instance ResponseErrorParam  'TextDocumentCodeLensK = Value

type CodeLens = Record
  '[ "range"   >: Range
   , "command" >: Option Command
   , "data"    >: Option Value
   ]

-- }}}

-- CodeLensResolve {{{
----------------------------------------
type instance RequestParam 'CodeLensResolveK = CodeLens
type instance ResponseResultParam 'CodeLensResolveK = CodeLens
type instance ResponseErrorParam  'CodeLensResolveK = Value

-- }}}

-- TextDocumentDocumentLink {{{
----------------------------------------
type instance RequestParam 'TextDocumentDocumentLinkK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   ]
type instance ResponseResultParam 'TextDocumentDocumentLinkK = Nullable DocumentLink
type instance ResponseErrorParam  'TextDocumentDocumentLinkK = Value

type DocumentLink = Record
  '[ "range"  >: Range
   , "target" >: Option Uri
   , "data"   >: Option Value
   ]
-- }}}

-- DocumentLinkResolve {{{
----------------------------------------
type instance RequestParam 'DocumentLinkResolveK = DocumentLink
type instance ResponseResultParam 'DocumentLinkResolveK = DocumentLink
type instance ResponseErrorParam  'DocumentLinkResolveK = Value
-- }}}

-- TextDocumentDocumentColor {{{
----------------------------------------
type instance RequestParam 'TextDocumentDocumentColorK = DocumentColorParams
type instance ResponseResultParam 'TextDocumentDocumentColorK = [ColorInformation]
type instance ResponseErrorParam  'TextDocumentDocumentColorK = Value
type ColorInformation = Record
  '[ "range" >: Range
   , "color" >: Color
   ]
type Color = Record
  '[ "red"   >: Number
   , "green" >: Number
   , "blue"  >: Number
   , "alpha" >: Number
   ]
type DocumentColorParams = Record
  '[ "textDocument" >: TextDocumentIdentifier
   ]
--}}}

-- TextDocumentColorPresentation {{{
----------------------------------------
type instance RequestParam 'TextDocumentColorPresentationK = ColorPresentationParams
type instance ResponseResultParam 'TextDocumentColorPresentationK = [ColorPresentation]
type instance ResponseErrorParam  'TextDocumentColorPresentationK = Value
type ColorPresentationParams = Record 
  '[ "textDocument" >: TextDocumentIdentifier
   , "color"        >: Color
   , "range"        >: Range
   ]
type ColorPresentation = Record
  '[ "label"               >: String
   , "textEdit"            >: Option TextEdit
   , "additionalTextEdits" >: Option [TextEdit]
   ]
--}}}

-- TextDocumentRename {{{
type instance RequestParam 'TextDocumentRenameK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "position"     >: Position
   , "newName"      >: String
   ]
type instance ResponseResultParam 'TextDocumentRenameK = Nullable WorkspaceEdit
type instance ResponseErrorParam  'TextDocumentRenameK = Value

type RenameRegistrationOptions = TextDocumentRegistrationOptions & Record
  '[ "prepareProvider" >: Option Bool
   ]

-- }}}

-- TODO
--type PrepareRename

-- Misc {{{
----------------------------------------
type instance RequestParam ('ClientRequestMiscK s) = Value
type instance ResponseResultParam ('ClientRequestMiscK s) = Value
type instance ResponseErrorParam  ('ClientRequestMiscK s) = Value

-- }}}

--}}}

-------------------------------------------------------------------------------
-- Client Notification --{{{
-------------------------------------------------------------------------------

-- Initialized {{{
type instance NotificationParam 'InitializedK = Record '[]
-- }}}

-- Exit{{{
----------------------------------------
type instance NotificationParam 'ExitK = Option Void

exitParam :: NotificationParam 'ExitK
exitParam = None

-- }}}

-- ClientCancel {{{
----------------------------------------
type instance NotificationParam 'ClientCancelK = Record '[ "id" >: ID ]

-- }}}

-- WorkspaceDidChangeConfiguration {{{
type instance NotificationParam 'WorkspaceDidChangeConfigurationK = Record
  '[ "settings" >: Value
   ]
-- }}}

-- WorkspaceDidChangeWatchedFiles {{{
type instance NotificationParam 'WorkspaceDidChangeWatchedFilesK = [FileEvent]
type FileEvent = Record
  '[ "uri"  >: Uri
   , "type" >: FileChangeType
   ]
type FileChangeType = EnumN
  '[ "created" >: 'Pos 1
   , "changed" >: 'Pos 2
   , "deleted" >: 'Pos 3
   ]

-- for dynamic registration
type DidChangeWatchedFilesRegistrationOptions = Record
  '[ "watchers" >: [FileSystemWatcher]
   ]
type FileSystemWatcher = Record
  '[ "globPattern" >: String
   , "kind" >: Option WatchKind
   ]
type WatchKind = EnumN
  '[ "create" >: 'Pos 1
   , "change" >: 'Pos 2
   , "delete" >: 'Pos 3
   ]
 
-- }}}

-- TextDocumentDidOpen {{{
----------------------------------------
type instance NotificationParam 'TextDocumentDidOpenK = Record
  '[ "textDocument" >: TextDocumentItem
   ]
didOpenTextDocumentParam :: TextDocumentItem
                         -> NotificationParam 'TextDocumentDidOpenK
didOpenTextDocumentParam textDocument = Record $ #textDocument @= textDocument <! nil

-- }}}

-- TextDocumentDidChange {{{
----------------------------------------
type instance NotificationParam 'TextDocumentDidChangeK = Record
  '[ "textDocument"   >: VersionedTextDocumentIdentifier
   , "contentChanges" >: [TextDocumentContentChangeEvent]
   ]
type TextDocumentContentChangeEvent = Record
  '[ "range"       >: Option Range
   , "rangeLength" >: Option Number
   , "text"        >: Text
   ]
-- }}}

-- TextDocumentWillSave {{{
type instance NotificationParam 'TextDocumentWillSaveK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "reason"       >: TextDocumentSaveReason
   ]
-- }}}

-- TextDocumentDidSave {{{
----------------------------------------
type instance NotificationParam 'TextDocumentDidSaveK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "text"         >: Option Text
   ]
-- }}}

-- TextDocumentDidClose {{{
----------------------------------------
type instance NotificationParam 'TextDocumentDidCloseK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   ]
-- }}}

-- DidChangeWorkspaceFolders {{{
----------------------------------------
type instance NotificationParam 'DidChangeWorkspaceFoldersK = DidChangeWorkspaceFoldersParam
type DidChangeWorkspaceFoldersParam = Record
  '[ "event" >: WorkspaceFoldersChangeEvent
   ]
type WorkspaceFoldersChangeEvent = Record
  '[ "added"   >: [WorkspaceFolder]
   , "removed" >: [WorkspaceFolder]
   ]

--}}}

-- Misc {{{
----------------------------------------
type instance NotificationParam ('ClientNotificationMiscK s) = Value
-- }}}

--}}}

-------------------------------------------------------------------------------
-- Server Request --{{{
-------------------------------------------------------------------------------

-- ClientRegisterCapability{{{
----------------------------------------
type instance RequestParam 'ClientRegisterCapabilityK = Record
  '[ "registrations" >: [Registration]
   ]
type instance ResponseResultParam 'ClientRegisterCapabilityK = Void
type instance ResponseErrorParam  'ClientRegisterCapabilityK = Value

type Registration = Record
  '[ "id"              >: String
   , "method"          >: String
   , "registerOptions" >: Option (TextDocumentRegistrationOptions :|: Value)
   ]
type TextDocumentRegistrationOptions = Record
  '[ "documentSelector" >: Nullable DocumentSelector
   ]
-- }}}

-- WorkspaceApplyEdit {{{
----------------------------------------
type instance RequestParam 'WorkspaceApplyEditK = ApplyWorkspaceEditParams
type instance ResponseResultParam 'WorkspaceApplyEditK = ApplyWorkspaceEditResponse
type instance ResponseErrorParam  'WorkspaceApplyEditK = String

type ApplyWorkspaceEditParams = Record
  '[ "label" >: Option String
   , "edit"  >: WorkspaceEdit
   ]
type ApplyWorkspaceEditResponse = Record
  '[ "applied" >: Bool
   ]
--}}}

-- WindowShowMessageRequest {{{
----------------------------------------
type instance RequestParam 'WindowShowMessageRequestK = Record
  '[ "type"    >: MessageType
   , "message" >: Text
   , "actions" >: Option [MessageActionItem]
   ]
type instance ResponseResultParam 'WindowShowMessageRequestK = Nullable MessageActionItem
type instance ResponseErrorParam  'WindowShowMessageRequestK = Value
--}}}

-- ClientUnregisterCapability {{{
----------------------------------------
type instance RequestParam 'ClientUnregisterCapabilityK = Record
  '[ "registrationParams" >: [Unregistration]
   ]
type instance ResponseResultParam 'ClientUnregisterCapabilityK = Void
type instance ResponseErrorParam  'ClientUnregisterCapabilityK = Value

type Unregistration = Record
  '[ "id"     >: String
   , "method" >: String
   ]
--}}}

-- WorkspaceFolders {{{
----------------------------------------
type instance RequestParam 'WorkspaceFoldersK = Void
type instance ResponseResultParam 'WorkspaceFoldersK = Nullable [WorkspaceFolder]
type instance ResponseErrorParam  'WorkspaceFoldersK = Value

type WorkspaceFolder = Record
  '[ "uri"  >: Uri
   , "name" >: String
   ]
--}}}

-- WorkspaceFolders {{{
----------------------------------------
type instance RequestParam 'WorkspaceConfigurationK = ConfigurationParams
type ConfigurationParams = Record
  '[ "items" >: [ConfigurationItem]
   ]
type ConfigurationItem = Record
  '[ "scopeUri" >: Option String
   , "section"  >: Option String
   ]
type instance ResponseResultParam 'WorkspaceConfigurationK = Nullable [WorkspaceFolder]
type instance ResponseErrorParam  'WorkspaceConfigurationK = Value

--}}}

-- Misc {{{
----------------------------------------
type instance RequestParam ('ServerRequestMiscK s) = Value
type instance ResponseResultParam ('ServerRequestMiscK s) = Value
type instance ResponseErrorParam  ('ServerRequestMiscK s) = Value
--}}}

--}}}

-------------------------------------------------------------------------------
-- Server Notification --{{{
-------------------------------------------------------------------------------

-- WindowShowMessageNotification {{{
type instance NotificationParam 'WindowShowMessageRequestK = Record
  '[ "type"    >: MessageType
   , "message" >: Text
   , "actions" >: Option [MessageActionItem]
   ]

type MessageActionItem = Record
  '[ "title" >: String ]
-- }}}

-- TextDocumentPublishDiagnostics {{{
----------------------------------------
type instance NotificationParam 'TextDocumentPublishDiagnosticsK = Record
  '[ "uri"         >: Uri
   , "diagnostics" >: [Diagnostic]
   ]
--}}}

-- WindowShowMessage {{{
----------------------------------------
type instance NotificationParam 'WindowShowMessageK = Record
  '[ "type"    >: MessageType
   , "message" >: Text
   ]
type MessageType = EnumN
  '[ "error"   >: 'Pos 1
   , "warning" >: 'Pos 2
   , "info"    >: 'Pos 3
   , "log"     >: 'Pos 4
   ]
--}}}

-- WindowLogMessage {{{
----------------------------------------
type instance NotificationParam 'WindowLogMessageK = Record
  '[ "type"    >: MessageType
   , "message" >: Text
   ]
--}}}

-- TelemetryEvent {{{
---------------------------------------
type instance NotificationParam 'TelemetryEventK = Value
--}}}

-- Cancel {{{
type instance NotificationParam 'ServerCancelK = Record '[ "id" >: ID ]
-- }}}

-- Misc {{{
----------------------------------------
type instance NotificationParam ('ServerNotificationMiscK s) = Value
--}}}

--}}}

