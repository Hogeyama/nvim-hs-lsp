
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
  , DiagnosticSeverity
  , TextDocumentSync
  , TextDocumentSyncKind
  , TextDocumentSyncOptions
  , Position
  , Range
  , Location
  , Diagnostic
  , Command
  , TextEdit
  , TextDocumentEdit
  , WorkspaceEdit
  , TextDocumentIdentifier
  , textDocumentIdentifier
  , VersionedTextDocmentIdentifier
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
-- Base Protocol
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
  '[ "code"    >: ErrorCode
   , "message" >: Text
   , "data"    >: Option e
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

-------------------------------------------------------------------------------
-- Common Data
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
type Location = Record
  '[ "uri"   >: Uri
   , "range" >: Range
   ]

-- Diagnostic
----------------------------------------
type Diagnostic = Record
  '[ "range"    >: Range
   , "severity" >: Option DiagnosticSeverity
   , "code"     >: Option (ErrorCode :|: String)
   , "source"   >: Option String
   , "message"  >: Text
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
  '[ "textDocument" >: VersionedTextDocmentIdentifier
   , "edits"        >: [TextEdit]
   ]

-- WorkspaceEdit
----------------------------------------
type WorkspaceEdit = Record
  '[ "changes"         >: Option (Map Uri [TextEdit])
   , "documentChanges" >: Option [TextDocumentEdit]
   ]

-- TextDocumentIdentifier
----------------------------------------
type TextDocumentIdentifier  = Record TextDocumentIdentifierF
type TextDocumentIdentifierF = '[ "uri" >: Uri ]

type VersionedTextDocmentIdentifier  = Record VersionedTextDocmentIdentifierF
type VersionedTextDocmentIdentifierF =
  TextDocumentIdentifierF ++
  '[ "version" >: Option Version
   ]

textDocumentIdentifier :: Uri -> TextDocumentIdentifier
textDocumentIdentifier uri = Record $ uri =<: nil

versionedTextDocmentIdentifier :: Uri -> Version -> VersionedTextDocmentIdentifier
versionedTextDocmentIdentifier uri version = Record (uri =<: Some version =<: nil)

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

-- ErrorCode
----------------------------------------

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

-- DiagnosticSeverity
----------------------------------------

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

type TextDocumentSyncKind = EnumN
  '[ "none"        >: 'Pos 0
   , "fulL"        >: 'Pos 1
   , "incremental" >: 'Pos 2
   ]

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
   ]

-- | Initialize Response > Result
type instance ResponseResultParam 'InitializeK = Record
  '[ "capabilities" >: ServerCapabilities
   ]

-- | Initialize Response > Error
type instance ResponseErrorParam 'InitializeK = Record
  '[ "retry" >: Bool
   ]

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

-- Other Data
--------------

type Trace = EnumS '[ "off", "messages", "verbose" ]

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
        '[ "documentChanges" >: Option Bool
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
   , "definition"         >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "typeDefinition"     >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "implementation"     >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "codeAction"         >: OptionalRecord
        '[ "dynamicRegistration" >: Option Bool
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
   ]
-- }}}

type MarkedString = String :|: Record
  '[ "language" >: String
   , "value"    >: String
   ]
type MarkupContent = Record
  '[ "kind"  >: MarkupKind
   , "value" >: String
   ]
type MarkupKind = EnumS '[ "plaintext", "markdown" ]

-- 後回し
type SymbolKind = Value
type CompletionItemKind = Value

type ServerCapabilities = Record
  '[ "textDocumentSync"                 >: Option TextDocumentSync
   , "hoverProvider"                    >: Option Bool
   , "completionProvider"               >: Option CompletionOptions
   , "signatureHelpProvider"            >: Option SignatureHelpOptions
   , "definitionProvider"               >: Option Bool
   , "referencesProvider"               >: Option Bool
   , "documentHighlightProvider"        >: Option Bool
   , "documentSymbolProvider"           >: Option Bool
   , "workspaceSymbolProvider"          >: Option Bool
   , "codeActionProvider"               >: Option Bool
   , "codeLensProvider"                 >: Option CodeLensOptions
   , "documentFormattingProvider"       >: Option Bool
   , "documentRangeFormattingProvider"  >: Option Bool
   , "documentOnTypeFormattingProvider" >: Option DocumentOnTypeFormattingOptions
   , "renameProvider"                   >: Option Bool
   , "documentLinkProvider"             >: Option DocumentLinkOptions
   , "executeCommandProvider"           >: Option ExecuteCommandOptions
   , "experimental"                     >: Option Value
   ]

type TextDocumentSync = TextDocumentSyncOptions :|: TextDocumentSyncKind

type TextDocumentSyncOptions = Record
  '[ "openClose"         >: Option Bool
   , "change"            >: Option TextDocumentSyncKind
   , "willSave"          >: Option Bool
   , "willSaveWaitUntil" >: Option Bool
   , "save"              >: Option SaveOptions
   ]
type SaveOptions = Record
  '[ "includeText" >: Option Bool
   ]

type CompletionOptions = Record
  '[ "resolveProvider"   >: Option Bool
   , "triggerCharacters" >: Option [String]
   ]
type SignatureHelpOptions = Record
  '[ "triggerCharacters" >: Option [String]
   ]
type CodeLensOptions = Record
  '[ "resolveProvider" >: Option Bool
   ]
type DocumentOnTypeFormattingOptions = Record
  '[ "firstTriggerCharacter" >: String
   , "moreTriggerCharacter"  >: Option [String]
   ]
type DocumentLinkOptions = Record
  '[ "resolveProvider" >: Option Bool
   ]
type ExecuteCommandOptions = Record
  '[ "commands" >: [String]
   ]

-- Shutdown {{{
----------------------------------------
type instance RequestParam 'ShutdownK = Option Void
type instance ResponseResultParam 'ShutdownK = Record '[]
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
type instance RequestParam 'DocumentLinkResolveK = DocumentLink
type instance ResponseResultParam 'DocumentLinkResolveK = DocumentLink
type instance ResponseErrorParam  'DocumentLinkResolveK = Value
-- }}}

-- TextDocumentRename {{{
type instance RequestParam 'TextDocumentRenameK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "position"     >: Position
   , "newName"      >: String
   ]
type instance ResponseResultParam 'TextDocumentRenameK = Nullable WorkspaceEdit
type instance ResponseErrorParam  'TextDocumentRenameK = Value

-- }}}

-- Misc {{{
----------------------------------------
type instance RequestParam ('ClientRequestMiscK s) = Value
type instance ResponseResultParam ('ClientRequestMiscK s) = Value
type instance ResponseErrorParam  ('ClientRequestMiscK s) = Value

-- }}}

--}}}

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
  '[ "textDocument"   >: VersionedTextDocmentIdentifier
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

-- Misc {{{
----------------------------------------
type instance NotificationParam ('ClientNotificationMiscK s) = Value
-- }}}

--}}}

-------------------------------------------------------------------------------
-- Server Request --{{{
-------------------------------------------------------------------------------

-- WindowShowMessageRequest {{{
type instance NotificationParam 'WindowShowMessageRequestK = Record
  '[ "type"    >: MessageType
   , "message" >: Text
   , "actions" >: Option [MessageActionItem]
   ]

type MessageActionItem = Record
  '[ "title" >: String ]
-- }}}

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
   , "registerOptions" >: Option Value
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

