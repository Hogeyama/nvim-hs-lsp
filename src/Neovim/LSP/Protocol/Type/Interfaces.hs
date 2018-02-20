
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
  , ClientMethod(..)
  , ServerMethod(..)

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

  -- type familyとか
  , X(..)
  , Method
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
import           Data.Singletons                 (SingI)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Map                        (Map)
import           GHC.Generics                    (Generic)
import           Neovim.LSP.Protocol.Type.JSON   (JSONField, Option (..))
import qualified Neovim.LSP.Protocol.Type.Key    as K
import           Neovim.LSP.Protocol.Type.Method
import           Safe                            (lookupJust)

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
type RequestMessage  (x :: X) a = Record (RequestMessageF (x :: X) a)
type RequestMessageF (x :: X) a =
  MessageF ++
  '[ "id"      >: ID
   , "method"  >: Method x
   , "params"  >: a -- NOTE 本来はOption a
                    -- ただmethodによってはomitされると困るので
                    -- それを表現するためにはRequestParamでOptionを付ける方が良い
                    -- と思ったのでそうする
                    -- 日本語
   ]

-- ResponseMessage
-----------------
type ResponseMessage  (x :: X) a e = Record (ResponseMessageF x a e)
type ResponseMessageF (x :: X) a e =
  MessageF ++
  '[ "id"      >: Nullable ID
   , "result"  >: a -- NOTE 本来はOption a
                    -- TODO 整合性とれてるか見直す
   , "error"   >: Option (ResponseError e) -- 同上
   ]

type ResponseError  e = Record (ResponseErrorF e)
type ResponseErrorF e =
  '[ "code"    >: ErrorCode
   , "message" >: Text
   , "data_"   >: e
   ]

-- Notification Message
-----------------------
type NotificationMessage  (x :: X) a = Record (NotificationMessageF x a)
type NotificationMessageF (x :: X) a =
  MessageF ++
  '[ "method"  >: Method x
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
  '[ "version" >: Version
   ]
-- Extend VersionedTextDocmentIdentifierF TextDocumentIdentifier

textDocumentIdentifier :: Uri -> TextDocumentIdentifier
textDocumentIdentifier uri = K.uri @= uri <: nil

versionedTextDocmentIdentifier :: Uri -> Version -> VersionedTextDocmentIdentifier
versionedTextDocmentIdentifier uri version = K.uri @= uri
                                          <: K.version @= version
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
      -- TODO
      -- ここにVersionedTextDocmentIdentifierが来てもいいはず
      -- shrinkという関数があるが，明示的に呼ばないといけない
      -- やっぱり型クラスでほげほげしないとダメか？
   , "position" >: Position
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
errorCodeTable = [
    (ParseError            , -32700)
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
  deriving (Show, Eq, Ord)

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

type RequestSyn      (m :: k) = RequestMessage      (Actor k) (RequestParam m)
type NotificationSyn (m :: k) = NotificationMessage (Actor k) (NotificationParam m)
type ResponseSyn     (m :: k) = ResponseMessage     (Actor k) (ResResult m) (ResError m)
newtype Request      (m :: k) = Request      (RequestSyn m)      deriving Generic
newtype Notification (m :: k) = Notification (NotificationSyn m) deriving Generic
newtype Response     (m :: k) = Response     (ResponseSyn m)     deriving Generic

type ClientResponse     (m :: ServerMethodK) = Response     m
type ClientRequest      (m :: ClientMethodK) = Request      m
type ClientNotification (m :: ClientMethodK) = Notification m
type ServerResponse     (m :: ClientMethodK) = Response     m
type ServerRequest      (m :: ServerMethodK) = Request      m
type ServerNotification (m :: ServerMethodK) = Notification m

type ImplRequest (m :: k) =
  (SingI m
  ,IsMethodKind k
  ,Show      (RequestParam m)
  ,JSONField (RequestParam m)
  )
type ImplResponse (m :: k) =
  (SingI m
  ,IsMethodKind k
  ,Show      (ResResult m)
  ,JSONField (ResResult m)
  ,Show      (ResError m)
  ,JSONField (ResError m)
  )
type ImplNotification (m :: k) =
  (SingI m
  ,IsMethodKind k
  ,Show      (NotificationParam m)
  ,JSONField (NotificationParam m)
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
   , "initializationOptions" >: Option ()  -- TODO
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

type ServerCapabilities = Record [
    "textDocumentSync"                 >: Option TextDocumentSync
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
type instance NotificationParam 'CancelRequestK = Record '[ "id" >: ID ]


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

-- s毎に分けるのはさすがに厳しそう
type instance RequestParam      ('MiscK s) = Value
type instance ResResult         ('MiscK s) = Value
type instance ResError          ('MiscK s) = Value

------------
-- Server --
------------


-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

revLookup :: Eq a => a -> [(b,a)] -> Maybe b
revLookup x dic = lookup x $ map flip' dic
  where flip' (a,b) = (b,a)

