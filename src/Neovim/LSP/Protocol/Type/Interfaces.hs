{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE UndecidableInstances   #-}
  -- (++)とJSONのderivingに必要．JSONの方はどうにかなるかも？
{-# OPTIONS_GHC -Wall               #-}

module Neovim.LSP.Protocol.Type.Interfaces
  ( Nullable
  , (:|:)(..)
  , ID(..)
  , Number
  , Version
  , Uri
  , ErrorCode(..)
  , DiagnosticSeverity(..)
  , TextDocumentSync(..)
  , TextDocumentSyncKind(..)
  , TextDocumentSyncOptions
  --, ClientMethod(..)
  --, ServerMethod(..)

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
  , DocumentFilter
  , DocumentSelector
  , Trace(..)

  -- type familyとか
  --, X(..)
  --, Method
  --
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

  -- function
  , filePathToUri
  , uriToFilePath
  , textDocumentIdentifier
  , versionedTextDocmentIdentifier
  -- reexport
  , Option(..)
  )
  where

import           Control.Applicative             ((<|>))
import           Control.Monad                   (mzero)
import           Data.Aeson                      hiding (Error)
import           Data.Char                       (digitToInt, toLower)
import           Data.Extensible                 hiding (Nullable)
import           Data.Hashable                   (Hashable)
import           Data.Singletons                 (SingI, SingKind(..))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Map                        (Map)
import           GHC.Generics                    (Generic)
import           Neovim.LSP.Protocol.Type.JSON   (FieldJSON, Option (..))
import           Neovim.LSP.Protocol.Type.Method
import           Safe                            (lookupJust)
import Data.Function (on)

-- Interfaces defined in
-- https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md

-------------------------------------------------------------------------------
-- Type for fields
-------------------------------------------------------------------------------

type Nullable = Maybe

-- | Sum type for record. The differnce against 'Either' is:
--
-- >>> :set -XOverloadedStrings -XTypeOperators
-- >>> encode (Left 1 :: Either Int Bool)
-- "{\"Left\":1}"
-- >>> encode (L 1 :: Int :|: Bool)
-- "1"
--
data (:|:) a b = L a | R b
  deriving (Show, Eq, Ord)
instance (FromJSON a, FromJSON b) => FromJSON (a :|: b) where
  parseJSON o =  L <$> parseJSON o
             <|> R <$> parseJSON o
instance (ToJSON a, ToJSON b) => ToJSON (a :|: b) where
  toJSON (L o) = toJSON o
  toJSON (R o) = toJSON o

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
   , "error"   >: Option (ResponseError e) -- 同上
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
type Version = Integer

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
  '[ "line"      >: Number
   , "character" >: Number
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
  '[ "changes"         >: Option (Map String [TextEdit])
      -- changes?: { [uri: string]: TextEdit[]; };
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
textDocumentIdentifier uri = #uri @= uri <: nil

versionedTextDocmentIdentifier :: Uri -> Version -> VersionedTextDocmentIdentifier
versionedTextDocmentIdentifier uri version = #uri @= uri
                                          <: #version @= Some version
                                          <: nil

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
type TextDocumentPositionParams = Record
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
  = ParseError            -- -32700
  | InvalidRequest        -- -32600
  | MethodNotFound        -- -32601
  | InvalidParams         -- -32602
  | InternalError         -- -32603
  | ServerErrorStart      -- -32099
  | ServerErrorEnd        -- -32000
  | ServerNotInitialized  -- -32002
  | UnknownErrorCode      -- -32001
  | RequestCancelled      -- -32800
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
  ,IsMethodKind k
  ,Show      (RequestParam m)
  ,FieldJSON (RequestParam m)
  )
type ImplResponse (m :: k) =
  (SingI m
  ,IsMethodKind k
  ,Show      (ResResult m)
  ,ToJSON    (ResResult m) -- TOOD: ResultはOptionで包むのでFieldJSONではダメ
  ,FromJSON  (ResResult m) --       綺麗に書けないかなあ
  ,Show      (ResError m)
  ,ToJSON    (ResError m)
  ,FromJSON  (ResError m)
  )
type ImplNotification (m :: k) =
  (SingI m
  ,IsMethodKind k
  ,Show      (NotificationParam m)
  ,FieldJSON (NotificationParam m)
  )

-- instances
-------------

deriving instance ImplRequest m => Show     (Request m)
deriving instance ImplRequest m => ToJSON   (Request m)
deriving instance ImplRequest m => FromJSON (Request m)

deriving instance ImplNotification m => Show     (Notification m)
deriving instance ImplNotification m => ToJSON   (Notification m)
deriving instance ImplNotification m => FromJSON (Notification m)

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

type WorkspaceClientCapabilities = Value

type TextDocumentClientCapabilities = Value

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
                      deriving (Show)

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

------------
-- Client --
------------

-- Exit
---------------------------------------
data Void
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

------------
-- Server --
------------

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

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

------------
-- Client --
------------

-- TextDocumentHover
---------------------------------------
type instance RequestParam 'TextDocumentHoverK = TextDocumentPositionParams
type instance ResResult    'TextDocumentHoverK = Hover
type instance ResError     'TextDocumentHoverK = Value
  --  "error: code and message set in case an exception happens during the hover request."

type Hover = Record
  '[ "contents" >: (MarkedString :|: [MarkedString])
   , "range"    >: Option Range
   ]
type MarkedString = String :|: Record
  '[ "language" >: String
   , "value"    >: String
   ]

-- TextDocumentSignatureHelp
---------------------------------------
type instance RequestParam 'TextDocumentSignatureHelpK = TextDocumentPositionParams
type instance ResResult    'TextDocumentSignatureHelpK = SignatureHelp
type instance ResError     'TextDocumentSignatureHelpK = Value

type SignatureHelp = Value
-- HIEが対応していないので後回しで良い

-- Misc
-------

type instance RequestParam      ('ClientRequestMiscK      s) = Value
type instance ResResult         ('ClientRequestMiscK      s) = Value
type instance ResError          ('ClientRequestMiscK      s) = Value
type instance NotificationParam ('ClientNotificationMiscK s) = Value

type instance RequestParam      ('ServerRequestMiscK      s) = Value
type instance ResResult         ('ServerRequestMiscK      s) = Value
type instance ResError          ('ServerRequestMiscK      s) = Value
type instance NotificationParam ('ServerNotificationMiscK s) = Value

------------
-- Server --
------------


-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

revLookup :: Eq a => a -> [(b,a)] -> Maybe b
revLookup x dic = lookup x $ map flip' dic
  where flip' (a,b) = (b,a)

