
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Neovim.LSP.Protocol.Type.Interfaces
  ( ID(..)
  , Number
  , Version
  , Uri(..)
  , ErrorCode--(..)
  , prettyResponceError
  , DiagnosticSeverity(..)
  , TextDocumentSync(..)
  , TextDocumentSyncKind--(..)
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
  , MessageType(..)
  , MessageActionItem

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
  , FormattingOptions(..)
  , SymbolInformation
  , DocumentSymbol(..)

  -- function
  , filePathToUri
  , uriToFilePath
  , textDocumentIdentifier
  , versionedTextDocmentIdentifier

  -- TODO
  , CodeAction
  , CodeActionKind
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
import           Data.Singletons                 (SingI, SingKind (..))
import           GHC.TypeLits
--import           Safe                              (lookupJust)

import           Data.Kind                       (Constraint)
import           Neovim.LSP.Protocol.Type.Method
import           Neovim.LSP.Protocol.Type.Record


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
prettyResponceError :: Show e => ResponseError e -> Text
prettyResponceError err =
    tshow (err^. #code) <> ": " <> err^. #message <> mdata
  where
    mdata = case err^. #data of
      None   -> mempty
      Some d -> ": " <> tshow d

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
textDocumentIdentifier uri = Record $ #uri @= uri <! nil

versionedTextDocmentIdentifier :: Uri -> Version -> VersionedTextDocmentIdentifier
versionedTextDocmentIdentifier uri version = Record
                                           $ #uri @= uri
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

-- |
-- >>> :m Data.List Neovim.LSP.Protocol.Type.Interfaces
-- >>> sort [Error, Warning, Hint]
-- [Error,Warning,Hint]
instance Ord DiagnosticSeverity where
  compare = compare `on` toInt
    where
      toInt = \case
        Error           -> 1
        Warning         -> 2
        Information     -> 3
        Hint            -> 4
        OtherSeverity m -> m


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
  ,ToJSON    (ResResult m) -- TODO: ResultはOptionで包むのでFieldJSONではダメ
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
type instance ResResult 'InitializeK = Record
  '[ "capabilities" >: ServerCapabilities
   ]

-- | Initialize Response > Error
type instance ResError 'InitializeK = Record
  '[ "retry" >: Bool
   ]

-- Other Data
--------------

--アホみたいにコンパイルに時間かかる
--type Trace = Enum' '[ "off", "messages", "verbose" ]
--traceOff, traceMessages, traceVerbose :: Trace
--traceOff = mkEnum' #off
--traceMessages = mkEnum' #messages
--traceVerbose = mkEnum' #verbose
data Trace = TraceOff | TraceMessages | TraceVerbose
  deriving (Show, Eq, Ord)
instance FromJSON Trace where
  parseJSON (String "off")      = return TraceOff
  parseJSON (String "messages") = return TraceMessages
  parseJSON (String "verbose")  = return TraceVerbose
  parseJSON _                   = mzero
instance ToJSON Trace where
  toJSON TraceOff      = String "off"
  toJSON TraceMessages = String "messages"
  toJSON TraceVerbose  = String "verbose"

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

--(To|From)JSON TextDocumentSync {{{
instance FromJSON TextDocumentSync where
  parseJSON x@Number{} = SyncKind   <$> parseJSON x
  parseJSON x          = SyncOption <$> parseJSON x
instance ToJSON TextDocumentSync where
  toJSON (SyncOption opt) = toJSON opt
  toJSON (SyncKind kind)  = toJSON kind
-- }}}
-- }}}

-- Shutdown {{{
----------------------------------------

-- TODO

-- }}}

-- Cancel {{{
----------------------------------------
type instance NotificationParam 'ClientCancelK = Record '[ "id" >: ID ]

-- }}}

-- TextDocumentHover {{{
----------------------------------------
type instance RequestParam 'TextDocumentHoverK = TextDocumentPositionParams
type instance ResResult    'TextDocumentHoverK = Nullable Hover
type instance ResError     'TextDocumentHoverK = Value
  --  "error: code and message set in case an exception happens during the hover request."

type Hover = Record
  '[ "contents" >: (MarkedString :|: [MarkedString] :|: MarkupContent)
   , "range"    >: Option Range
   ]

-- }}}

-- TextDocumentSignatureHelp {{{
----------------------------------------
type instance RequestParam 'TextDocumentSignatureHelpK = TextDocumentPositionParams
type instance ResResult    'TextDocumentSignatureHelpK = SignatureHelp
type instance ResError     'TextDocumentSignatureHelpK = Value

type SignatureHelp = Value
  -- HIEが対応していないので後回しで良い

-- }}}

-- TextDocumentDefinition {{{
----------------------------------------
type instance RequestParam 'TextDocumentDefinitionK = TextDocumentPositionParams
type instance ResResult    'TextDocumentDefinitionK = Nullable [Location]
type instance ResError     'TextDocumentDefinitionK = String

-- }}}

-- WorkspaceExecuteCommand {{{
----------------------------------------
type instance RequestParam 'WorkspaceExecuteCommandK = ExecuteCommandParams
type instance ResResult    'WorkspaceExecuteCommandK = Nullable Value
type instance ResError     'WorkspaceExecuteCommandK = String

type ExecuteCommandParams = Record
  '[ "command"   >: String
   , "arguments" >: Option [Value]
   ]

-- }}}

-- WorkspaceSymbol {{{
----------------------------------------
type instance RequestParam 'WorkspaceSymbolK = Record
  '[ "query" >: String ]
type instance ResResult    'WorkspaceSymbolK = Nullable [SymbolInformation]
type instance ResError     'WorkspaceSymbolK = Value

-- }}}

-- TextDocumentCompletion {{{
----------------------------------------
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

-- }}}

-- TextDocumentReferences {{{
----------------------------------------
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

-- }}}

-- TextDocumentCodeAction {{{
----------------------------------------

type instance RequestParam 'TextDocumentCodeActionK = CodeActionParams
type instance ResResult 'TextDocumentCodeActionK = Nullable [Command :|: CodeAction]
type instance ResError  'TextDocumentCodeActionK = CodeActionParams

type CodeActionParams = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "range"        >: Range
   , "context"      >: CodeActionContext
   ]
type CodeActionContext = Record
  '[ "diagnostics" >: [Diagnostic]
   , "only"        >: Option [CodeActionKind]
   ]

type CodeActionKind = Enum'
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

-- TextDocumentDocumentSymbol {{{
----------------------------------------
type instance RequestParam 'TextDocumentDocumentSymbolK = Record
  '[ "textDocument" >: TextDocumentIdentifier ]
type instance ResResult 'TextDocumentDocumentSymbolK = Nullable ([DocumentSymbol] :|: [SymbolInformation])
type instance ResError  'TextDocumentDocumentSymbolK = Value

newtype DocumentSymbol = DocumentSymbol (Record
  '[ "name" >: String
   , "detail" >: Option String
   , "kind" >: SymbolKind
   , "deprecated" >: Option Bool
   , "range" >: Range
   , "children" >: Option [DocumentSymbol]
   ])
  deriving (Show,Eq,ToJSON,FromJSON)

type SymbolInformation = Record
  '[ "name" >: String
   , "kind" >: SymbolKind -- specificationだとnumberってなってるけどたぶんこれで合ってる
   , "deprecated" >: Option Bool
   , "location" >: Location
   , "containerName" >: Option String
   ]

--}}}

-- TextDocumentFormatting {{{
----------------------------------------
type instance RequestParam 'TextDocumentFormattingK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "options" >: FormattingOptions
   ]
type instance ResResult    'TextDocumentFormattingK = Nullable [TextEdit]
type instance ResError     'TextDocumentFormattingK = String

-- FormattingOptions {{{
-- |
-- Original difinition is:
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
type family FOField' x :: Constraint where
  FOField' Number = ()
  FOField' Bool = ()
  FOField' String = ()
  FOField' x = TypeError ('Text "damedesu") -- TODO
class FOField' x => FOField x
instance FOField' x => FOField x
-- }}}

-- }}}

-- TextDocumentRangeFormatting {{{
----------------------------------------
type instance RequestParam 'TextDocumentRangeFormattingK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "range" >: Range
   , "options" >: FormattingOptions
   ]
type instance ResResult    'TextDocumentRangeFormattingK = Nullable [TextEdit]
type instance ResError     'TextDocumentRangeFormattingK = String

--}}}

-- Misc {{{
----------------------------------------

type instance RequestParam ('ClientRequestMiscK s) = Value
type instance ResResult    ('ClientRequestMiscK s) = Value
type instance ResError     ('ClientRequestMiscK s) = Value

-- }}}

--}}}

-------------------------------------------------------------------------------
-- Client Notification --{{{
-------------------------------------------------------------------------------

-- Exit
----------------------------------------
data Void
instance Eq       Void where _ == _      = True
instance Show     Void where show        = \case{}
instance ToJSON   Void where toJSON      = \case{}
instance FromJSON Void where parseJSON _ = mempty

type instance NotificationParam 'ExitK = Option Void

-- TextDocumentDidOpen
----------------------------------------
type instance NotificationParam 'TextDocumentDidOpenK = Record
  '[ "textDocument" >: TextDocumentItem
   ]

-- TextDocumentDidClose
----------------------------------------
type instance NotificationParam 'TextDocumentDidCloseK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   ]

-- TextDocumentDidChange
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
  -- None,None,FullTextにすればよい

-- TextDocumentDidSave
----------------------------------------
type instance NotificationParam 'TextDocumentDidSaveK = Record
  '[ "textDocument" >: TextDocumentIdentifier
   , "text"         >: Option Text
   ]

-- Misc
----------------------------------------
type instance NotificationParam ('ClientNotificationMiscK s) = Value

--}}}

-------------------------------------------------------------------------------
-- Server Notification --{{{
-------------------------------------------------------------------------------

-- TextDocumentPublishDiagnostics
----------------------------------------
type instance NotificationParam 'TextDocumentPublishDiagnosticsK = Record
  '[ "uri"         >: Uri
   , "diagnostics" >: [Diagnostic]
   ]

-- WindowShowMessage
----------------------------------------
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
----------------------------------------
type instance NotificationParam 'WindowLogMessageK = Record
  '[ "type"    >: MessageType
   , "message" >: Text
   ]

-- TelemetryEvent
----------------------------------------
type instance NotificationParam 'TelemetryEventK = Value

-- Misc
----------------------------------------
type instance NotificationParam ('ServerNotificationMiscK s) = Value

--}}}

-------------------------------------------------------------------------------
-- Server Request --{{{
-------------------------------------------------------------------------------

-- WorkspaceApplyEdit
----------------------------------------
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

-- WindowShowMessageRequest
----------------------------------------
type instance RequestParam 'WindowShowMessageRequestK = Record
  '[ "type"    >: MessageType
   , "message" >: Text
   , "actions" >: Option [MessageActionItem]
   ]
type instance ResResult 'WindowShowMessageRequestK = Nullable MessageActionItem
type instance ResError  'WindowShowMessageRequestK = Value -- TODO

type MessageActionItem = Record
  '[ "title" >: String ]

-- ClientRegisterCapability
----------------------------------------
type instance RequestParam 'ClientRegisterCapabilityK = Record
  '[ "registrationParams" >: [Registration]
   ]
type instance ResResult 'ClientRegisterCapabilityK = Void
type instance ResError  'ClientRegisterCapabilityK = Value -- TODO

type Registration = Record
  '[ "id" >: String
   , "method" >: String
   , "registerOptions" >: Option Value
   ]

-- ClientUnregisterCapability
----------------------------------------
type instance RequestParam 'ClientUnregisterCapabilityK = Record
  '[ "registrationParams" >: [Unregistration]
   ]
type instance ResResult 'ClientUnregisterCapabilityK = Void
type instance ResError  'ClientUnregisterCapabilityK = Value -- TODO

type Unregistration = Record
  '[ "id" >: String
   , "method" >: String
   ]

-- Misc
----------------------------------------
type instance RequestParam ('ServerRequestMiscK s) = Value
type instance ResResult    ('ServerRequestMiscK s) = Value
type instance ResError     ('ServerRequestMiscK s) = Value

--}}}

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

revLookup :: Eq a => a -> [(b,a)] -> Maybe b
revLookup x = lookup x . map swap
  where swap (a,b) = (b,a)

