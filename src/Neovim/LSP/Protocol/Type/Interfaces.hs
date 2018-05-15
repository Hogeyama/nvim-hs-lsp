
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall               #-}

module Neovim.LSP.Protocol.Type.Interfaces
  ( Nullable
  , (:|:)(..)
  , ID(..)
  , Number
  , Version
  , Uri(..)
  , ErrorCode(..)
  , DiagnosticSeverity(..)
  , TextDocumentSync(..)
  , TextDocumentSyncKind(..)
  , TextDocumentSyncOptions

  , Message
  , RequestMessage
  , ResponseMessage
  , ResponseError
  , NotificationMessage

  , Position
  , Range
  , Location
  , Diagnostic
  , Command
  , TextEdit
  , TextDocumentEdit
  , WorkspaceEdit
  , TextDocumentIdentifier
  , VersionedTextDocmentIdentifier
  , TextDocumentItem
  , TextDocumentPositionParams
  , CompletionItem
  , CompletionList
  , DocumentFilter
  , DocumentSelector
  , Trace(..)

  , RequestParam
  , NotificationParam
  , ResResult
  , ResError
  --
  , Response     (..)
  , Request      (..)
  , Notification (..)
  , ImplNotification
  , ImplRequest
  , ImplResponse
  --
  , ClientResponse
  , ClientRequest
  , ClientNotification
  , ServerResponse
  , ServerRequest
  , ServerNotification
  --
  --, Sing(..)
  --, isServerRequest
  --, isServerNotification
  --, isServerResponse

  -- 他のdata型
  , Hover
  , MarkedString
  , MarkupKind(..)
  , MarkupContent
  , ApplyWorkspaceEditResponse
  , WorkspaceClientCapabilities
  , TextDocumentClientCapabilities

  -- function
  , filePathToUri
  , uriToFilePath
  , textDocumentIdentifier
  , versionedTextDocmentIdentifier
  -- reexport
  , Option(..)
  )
  where

import           Control.Monad                     (mzero)
import           Data.Aeson                        hiding (Error)
import           Data.Aeson.Types                  (toJSONKeyText)
import           Data.Char                         (digitToInt, toLower)
import           Data.Extensible                   hiding (Nullable)
import           Data.Function                     (on)
import           Data.Hashable                     (Hashable)
import           Data.Map                          (Map)
import           Data.Singletons                   (SingI, SingKind (..))
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Typeable
import           GHC.Generics                      (Generic)
import           Neovim.LSP.Protocol.Type.Instance
import           Neovim.LSP.Protocol.Type.Method
import           Safe                              (lookupJust)

-- Interfaces defined in
-- https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md

-------------------------------------------------------------------------------
-- Base Protocol
-------------------------------------------------------------------------------

-- Message
----------
type Message = Record MessageF
type MessageF = '[ "jsonrpc" >: String ]

-- RequestMessage
-----------------
type RequestMessage  m a = Record (RequestMessageF m a)
type RequestMessageF m a =
  MessageF ++
  '[ "id"      >: ID
   , "method"  >: m
   , "params"  >: a -- NOTE 本来はOption a
                    -- ただmethodによってはomitされると困るので
                    -- それを表現するためにはRequestParamでOptionを付ける方が良い
                    -- と思ったのでそうする
                    -- 日本語
   ]

-- ResponseMessage
-----------------
type ResponseMessage  a e = Record (ResponseMessageF a e)
type ResponseMessageF a e =
  MessageF ++
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

-- Notification Message
-----------------------
type NotificationMessage  m a = Record (NotificationMessageF m a)
type NotificationMessageF m a =
  MessageF ++
  '[ "method"  >: m
   , "params"  >: a  -- NOTE 本来はOption a
   ]

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
  deriving (Show, Eq, Ord, Generic, Hashable)
instance FromJSON ID where
  parseJSON (Number n) = return $ IDNum $ realToFrac n
  parseJSON (String s) = return $ IDString $ T.unpack s
  parseJSON _          = mzero
instance ToJSON ID where
  toJSON (IDNum n)    = toJSON n
  toJSON (IDString s) = toJSON s

-- Uri
----------------------------------------

newtype Uri = Uri { getUri :: Text }
  deriving (Eq,Ord,Read,Show,Generic,Hashable)
instance ToJSON Uri where
  toJSON = toJSON . getUri
instance FromJSON Uri where
  parseJSON o = Uri <$> parseJSON o
instance ToJSONKey Uri where
  toJSONKey = toJSONKeyText getUri
instance FromJSONKey Uri where
  fromJSONKey = FromJSONKeyText Uri

-- TODO test
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

-- TODO test
filePathToUri :: FilePath -> Uri
filePathToUri (drive:':':rest) = Uri $
    T.pack $ concat ["file:///", [toLower drive], "%3A", fmap convertDelim rest]
  where
    convertDelim '\\' = '/'
    convertDelim c    = c
filePathToUri file = Uri $ T.pack $ "file://" ++ file

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
   , "newText" >: String
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
textDocumentIdentifier uri = #uri @= uri <! nil

versionedTextDocmentIdentifier :: Uri -> Version -> VersionedTextDocmentIdentifier
versionedTextDocmentIdentifier uri version = #uri @= uri
                                          <! #version @= Some version
                                          <! nil

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

data ErrorCode
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerErrorStart
  | ServerErrorEnd
  | ServerNotInitialized
  | UnknownErrorCode
  | RequestCancelled
  | OtherError Int
  deriving (Show, Eq, Ord)

errorCodeTable :: [(ErrorCode, Int)]
errorCodeTable =
  [ (ParseError            , -32700)
  , (InvalidRequest        , -32600)
  , (MethodNotFound        , -32601)
  , (InvalidParams         , -32602)
  , (InternalError         , -32603)
  , (ServerErrorStart      , -32099)
  , (ServerErrorEnd        , -32000)
  , (ServerNotInitialized  , -32002)
  , (UnknownErrorCode      , -32001)
  , (RequestCancelled      , -32800)
  ]

instance FromJSON ErrorCode where
  parseJSON (Number n) = case revLookup (round n) errorCodeTable of
    Just err -> return err
    Nothing  -> return (OtherError (round n))
  parseJSON _ = mzero

instance ToJSON ErrorCode where
  toJSON (OtherError n) = Number $ fromIntegral n
  toJSON err = Number $ fromIntegral $ lookupJust err errorCodeTable

-- DiagnosticSeverity
----------------------------------------

data DiagnosticSeverity
  = Error
  | Warning
  | Information
  | Hint
  | OtherSeverity Int
  deriving (Show, Eq)

toInt :: DiagnosticSeverity -> Int
toInt = \case
  Error           -> 1
  Warning         -> 2
  Information     -> 3
  Hint            -> 4
  OtherSeverity m -> m

-- |
-- >>> :m Data.List Neovim.LSP.Protocol.Type.Interfaces
-- >>> sort [Error, Warning, Hint]
-- [Error,Warning,Hint]
instance Ord DiagnosticSeverity where
  compare = compare `on` toInt

instance FromJSON DiagnosticSeverity where
  parseJSON (Number n) = return $ case round n of
      1 -> Error
      2 -> Warning
      3 -> Information
      4 -> Hint
      m -> OtherSeverity m
  parseJSON _ = mzero
instance ToJSON DiagnosticSeverity where
  toJSON Error             = toJSON (1 :: Int)
  toJSON Warning           = toJSON (2 :: Int)
  toJSON Information       = toJSON (3 :: Int)
  toJSON Hint              = toJSON (4 :: Int)
  toJSON (OtherSeverity m) = toJSON m

data TextDocumentSyncKind = SynkNone | SynkFull | SynkIncremental
  deriving (Show, Eq, Ord)

instance FromJSON TextDocumentSyncKind where
  parseJSON (Number n) = case round @_ @Int n of
      0 -> return SynkNone
      1 -> return SynkFull
      2 -> return SynkIncremental
      _ -> mzero
  parseJSON _ = mzero
instance ToJSON TextDocumentSyncKind where
  toJSON SynkNone        = Number 0
  toJSON SynkFull        = Number 1
  toJSON SynkIncremental = Number 2

-------------------------------------------------------------------------------
--- Param
-------------------------------------------------------------------------------

type family RequestParam      (m :: k)
type family NotificationParam (m :: k)
type family ResResult         (m :: k)
type family ResError          (m :: k)

type RequestSyn      (m :: k) = RequestMessage      (Demote k) (RequestParam m)
type NotificationSyn (m :: k) = NotificationMessage (Demote k) (NotificationParam m)
type ResponseSyn     (m :: k) = ResponseMessage     (ResResult m) (ResError m)
newtype Request      (m :: k) = Request      (RequestSyn m)      deriving Generic
newtype Notification (m :: k) = Notification (NotificationSyn m) deriving Generic
newtype Response     (m :: k) = Response     (ResponseSyn m)     deriving Generic

type ClientResponse     (m :: ServerRequestMethodK)      = Response     m
type ClientRequest      (m :: ClientRequestMethodK)      = Request      m
type ClientNotification (m :: ClientNotificationMethodK) = Notification m
type ServerResponse     (m :: ClientRequestMethodK)      = Response     m
type ServerRequest      (m :: ServerRequestMethodK)      = Request      m
type ServerNotification (m :: ServerNotificationMethodK) = Notification m

type ImplRequest (m :: k) =
  (SingI m
  ,Typeable m
  ,IsMethodKind k
  ,Eq        (RequestParam m)
  ,Show      (RequestParam m)
  ,FieldJSON (RequestParam m)
  )
type ImplResponse (m :: k) =
  (SingI m
  ,Typeable m
  ,IsMethodKind k
  ,Eq        (ResResult m)
  ,Show      (ResResult m)
  ,ToJSON    (ResResult m) -- TOOD: ResultはOptionで包むのでFieldJSONではダメ
  ,FromJSON  (ResResult m) --       綺麗に書けないかなあ
  ,Eq        (ResError m)
  ,Show      (ResError m)
  ,ToJSON    (ResError m)
  ,FromJSON  (ResError m)
  )
type ImplNotification (m :: k) =
  (SingI m
  ,Typeable m
  ,IsMethodKind k
  ,Eq        (NotificationParam m)
  ,Show      (NotificationParam m)
  ,FieldJSON (NotificationParam m)
  )

-- instances
-------------

deriving instance ImplRequest m => Eq       (Request m)
deriving instance ImplRequest m => Show     (Request m)
deriving instance ImplRequest m => ToJSON   (Request m)
deriving instance ImplRequest m => FromJSON (Request m)

deriving instance ImplNotification m => Eq       (Notification m)
deriving instance ImplNotification m => Show     (Notification m)
deriving instance ImplNotification m => ToJSON   (Notification m)
deriving instance ImplNotification m => FromJSON (Notification m)

deriving instance ImplResponse m => Eq       (Response m)
deriving instance ImplResponse m => Show     (Response m)
deriving instance ImplResponse m => ToJSON   (Response m)
deriving instance ImplResponse m => FromJSON (Response m)

-- proof
--------

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

-- Initialize
---------------------------------------

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
type instance ResResult 'InitializeK = Record
  '[ "capabilities" >: ServerCapabilities
   ]

-- | Initialize Response > Error
type instance ResError 'InitializeK = Record
  '[ "retry" >: Bool
   ]

-- Other Data
--------------

data Trace = TraceOff | TraceMessages | TraceVerbose
  deriving (Show, Eq, Ord)

type ClientCapabilities = Record
  '[ "workspace"    >: Option WorkspaceClientCapabilities
   , "textDocument" >: Option TextDocumentClientCapabilities
   , "experimental" >: Option Value
   ]

-- TODO move
type OptionRecord xs = Option (Record xs)

type WorkspaceClientCapabilities = Record
  -- {{{
  '[ "applyEdit" >: Option Bool
   , "workspaceEdit" >: OptionRecord
        '[ "documentChanges" >: Option Bool
         ]
   , "didChangeConfiguration" >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "didChangeWatchedFiles" >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "symbol" >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         , "symbolKind" >: OptionRecord
              '[ "valueSet" >: Option [SymbolKind]
               ]
         ]
   , "executeCommand" >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "workspaceFolders" >: Option Bool
   , "configuration" >: Option Bool
   ]
  -- }}}

type TextDocumentClientCapabilities = Record
  -- {{{
  '[ "synchronization"    >: OptionRecord
        '[ "dynamicRegistration"  >: Option Bool
         , "willSave"             >: Option Bool
         , "willSaveUntil"        >: Option Bool
         , "didSave"              >: Option Bool
         ]
   , "completion"         >: OptionRecord
        '[ "dynamicRegistration"  >: Option Bool
         , "completionItem"       >: OptionRecord
              '[ "snippetSupport"          >: Option Bool
               , "commitCharactersSupport" >: Option Bool
               , "documentationFormat"     >: Option [MarkupKind]
               ]
         , "completionItemKind"   >: OptionRecord
              '[ "valueSet" >: Option [CompletionItemKind]
               ]
         , "contextSupport"       >: Option Bool
         ]
   , "hover"              >: OptionRecord
        '[ "dynamicRegistration"  >: Option Bool
         , "contentFormat"        >: Option [MarkupKind]
         ]
   , "signatureHelp"      >: OptionRecord
        '[ "dynamicRegistration"  >: Option Bool
         , "signatureInformation" >: OptionRecord
              '[ "documentationFormat" >: Option [MarkupKind]
               ]
         ]
   , "references"         >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "documentHightlight" >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "documentSymbol"     >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         , "symbolKind"          >: OptionRecord
              '[ "valueSet" >: Option [SymbolKind]
               ]
         ]
   , "formatting"         >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "rangeFormatting"    >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "onTypeFormatting"   >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "definition"         >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "typeDefinition"     >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "implementation"     >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "codeAction"         >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "codeLens"           >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "documentLink"       >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "colorProvider"      >: OptionRecord
        '[ "dynamicRegistration" >: Option Bool
         ]
   , "rename"             >: OptionRecord
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
data MarkupKind = PlainText | Markdown
  deriving (Show, Eq, Ord)
instance FromJSON MarkupKind where
  parseJSON (String "plaintext") = return PlainText
  parseJSON (String "markdown")  = return Markdown
  parseJSON _                    = mempty
instance ToJSON MarkupKind where
  toJSON PlainText = String "plaintext"
  toJSON Markdown  = String "markdown"

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

data TextDocumentSync = SyncOption TextDocumentSyncOptions
                      | SyncKind   TextDocumentSyncKind
                      deriving (Eq,Ord,Show)

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

-- instances
-------------
-- (To|From)JSON Trace{{{
instance FromJSON Trace where
  parseJSON (String "off")      = return TraceOff
  parseJSON (String "messages") = return TraceMessages
  parseJSON (String "verbose")  = return TraceVerbose
  parseJSON _                   = mzero
instance ToJSON Trace where
  toJSON TraceOff      = String "off"
  toJSON TraceMessages = String "messages"
  toJSON TraceVerbose  = String "verbose"
-- }}}

--(To|From)JSON TextDocumentSync{{{
instance FromJSON TextDocumentSync where
  parseJSON x@Number{} = SyncKind   <$> parseJSON x
  parseJSON x          = SyncOption <$> parseJSON x
instance ToJSON TextDocumentSync where
  toJSON (SyncOption opt) = toJSON opt
  toJSON (SyncKind kind)  = toJSON kind
-- }}}

-- Shutdown
---------------------------------------

-- Cancel
---------------------------------------
type instance NotificationParam 'ClientCancelK = Record '[ "id" >: ID ]


-------------------------------------------------------------------------------
-- Notification
-------------------------------------------------------------------------------

---------------------------------------
-- Client                            --
---------------------------------------

-- Exit
---------------------------------------
data Void
instance Eq       Void where _ == _      = True
instance Show     Void where show        = \case{}
instance ToJSON   Void where toJSON      = \case{}
instance FromJSON Void where parseJSON _ = mempty

type instance NotificationParam 'ExitK = Option Void

-- TextDocumentDidOpen
---------------------------------------
type instance NotificationParam 'TextDocumentDidOpenK = Record
  '[ "textDocument" >: TextDocumentItem
   ]

-- TextDocumentDidClose
---------------------------------------
type instance NotificationParam 'TextDocumentDidCloseK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   ]

-- TextDocumentDidChange
---------------------------------------
type instance NotificationParam 'TextDocumentDidChangeK = Record
  '[ "textDocument"   >: VersionedTextDocmentIdentifier
   , "contentChanges" >: [TextDocumentContentChangeEvent]
   ]
type TextDocumentContentChangeEvent = Record
  '[ "range"       >: Option Range
   , "rangeLength" >: Option Number
   , "text"        >: Text
   ]
  -- None,None,FullTextにすればよい

-- TextDocumentDidSave
---------------------------------------
type instance NotificationParam 'TextDocumentDidSaveK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "text"         >: Option Text
   ]

-- Misc
---------------------------------------
type instance NotificationParam ('ClientNotificationMiscK s) = Value

---------------------------------------
-- Server                            --
---------------------------------------

-- TextDocumentPublishDiagnostics
---------------------------------------
type instance NotificationParam 'TextDocumentPublishDiagnosticsK = Record
  '[ "uri"         >: Uri
   , "diagnostics" >: [Diagnostic]
   ]

-- WindowShowMessage
---------------------------------------
type instance NotificationParam 'WindowShowMessageK = Record
  '[ "type"    >: MessageType
   , "message" >: Text
   ]

data MessageType
  = MessageError
  | MessageWarning
  | MessageInfo
  | MessageLog
  deriving (Show,Eq,Ord)
instance ToJSON MessageType where-- {{{
  toJSON MessageError   = Number 1
  toJSON MessageWarning = Number 2
  toJSON MessageInfo    = Number 3
  toJSON MessageLog     = Number 4
instance FromJSON MessageType where
  parseJSON (Number n) = case round @_ @Int n of
    1 -> pure MessageError
    2 -> pure MessageWarning
    3 -> pure MessageInfo
    4 -> pure MessageLog
    _ -> mzero
  parseJSON _ = mzero
-- }}}

-- WindowLogMessage
---------------------------------------
type instance NotificationParam 'WindowLogMessageK = Record
  '[ "type"    >: MessageType
   , "message" >: Text
   ]

-- TelemetryEvent
---------------------------------------
type instance NotificationParam 'TelemetryEventK = Value

-- Misc
---------------------------------------
type instance NotificationParam ('ServerNotificationMiscK s) = Value

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

---------------------------------------
-- Client                            --
---------------------------------------

-- TextDocumentHover
---------------------------------------
type instance RequestParam 'TextDocumentHoverK = TextDocumentPositionParams
type instance ResResult    'TextDocumentHoverK = Nullable Hover
type instance ResError     'TextDocumentHoverK = Value
  --  "error: code and message set in case an exception happens during the hover request."

type Hover = Record
  '[ "contents" >: (MarkedString :|: [MarkedString] :|: MarkupContent)
   , "range"    >: Option Range
   ]

-- TextDocumentSignatureHelp
---------------------------------------
type instance RequestParam 'TextDocumentSignatureHelpK = TextDocumentPositionParams
type instance ResResult    'TextDocumentSignatureHelpK = SignatureHelp
type instance ResError     'TextDocumentSignatureHelpK = Value

type SignatureHelp = Value
-- HIEが対応していないので後回しで良い

-- TextDocumentDefinition
---------------------------------------
type instance RequestParam 'TextDocumentDefinitionK = TextDocumentPositionParams
type instance ResResult    'TextDocumentDefinitionK = Nullable [Location]
type instance ResError     'TextDocumentDefinitionK = String

-- WorkspaceExecuteCommand
---------------------------------------
type instance RequestParam 'WorkspaceExecuteCommandK = ExecuteCommandParams
type instance ResResult    'WorkspaceExecuteCommandK = Nullable Value
type instance ResError     'WorkspaceExecuteCommandK = String

type ExecuteCommandParams = Record
  '[ "command"   >: String
   , "arguments" >: Option [Value]
   ]

-- TextDocumentCompletion
---------------------------------------
type instance RequestParam 'TextDocumentCompletionK = CompletionParams
type instance ResResult    'TextDocumentCompletionK = Nullable ([CompletionItem] :|: CompletionList)
type instance ResError     'TextDocumentCompletionK = String

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

-- TextDocumentReferences
---------------------------------------
type instance RequestParam 'TextDocumentReferencesK = ReferenceParams
type instance ResResult    'TextDocumentReferencesK = Nullable [Location]
type instance ResError     'TextDocumentReferencesK = String

type ReferenceParams  = Record ReferenceParamsF
type ReferenceParamsF =
  TextDocumentPositionParamsF ++
  '[ "context" >: ReferenceContext
   ]
type ReferenceContext = Record
  '[ "includeDeclaration" >: Bool
   ]

-- Misc
---------------------------------------

type instance RequestParam      ('ClientRequestMiscK      s) = Value
type instance ResResult         ('ClientRequestMiscK      s) = Value
type instance ResError          ('ClientRequestMiscK      s) = Value

---------------------------------------
-- Server                            --
---------------------------------------

-- WorkspaceApplyEdit
---------------------------------------
type instance RequestParam 'WorkspaceApplyEditK = ApplyWorkspaceEditParams
type instance ResResult    'WorkspaceApplyEditK = ApplyWorkspaceEditResponse
type instance ResError     'WorkspaceApplyEditK = String

type ApplyWorkspaceEditParams = Record
  '[ "label" >: Option String
   , "edit"  >: WorkspaceEdit
   ]
type ApplyWorkspaceEditResponse = Record
  '[ "applied" >: Bool
   ]

-- Misc
---------------------------------------
type instance RequestParam ('ServerRequestMiscK s) = Value
type instance ResResult    ('ServerRequestMiscK s) = Value
type instance ResError     ('ServerRequestMiscK s) = Value


-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

revLookup :: Eq a => a -> [(b,a)] -> Maybe b
revLookup x dic = lookup x $ map flip' dic
  where flip' (a,b) = (b,a)

