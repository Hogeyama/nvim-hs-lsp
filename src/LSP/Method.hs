
{-# OPTIONS_GHC -Wall #-}

module LSP.Method
  ( ClientRequestMethod (..), ClientNotificationMethod (..)
  , ClientRequestMethodK(..), ClientNotificationMethodK(..)
  , ServerRequestMethod (..), ServerNotificationMethod (..)
  , ServerRequestMethodK(..), ServerNotificationMethodK(..)
  , Sing(..)
  , IsMethodKind
  )
  where

import           RIO
import qualified RIO.Text                          as T

import           Data.Aeson
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           LSP.Record

-------------------------------------------------------------------------------
-- Method
-------------------------------------------------------------------------------

data ClientRequestMethod-- {{{
  = Initialize
  | Shutdown
  | WorkspaceSymbol
  | WorkspaceExecuteCommand
  | TextDocumentWillSaveWaitUntil
  | TextDocumentCompletion
  | CompletionItemResolve
  | TextDocumentHover
  | TextDocumentSignatureHelp
  | TextDocumentReferences
  | TextDocumentDocumentHighlight
  | TextDocumentDocumentSymbol
  | TextDocumentFormatting
  | TextDocumentRangeFormatting
  | TextDocumentOnTypeFormatting
  | TextDocumentDefinition
  | TextDocumentCodeAction
  | TextDocumentCodeLens
  | CodeLensResolve
  | TextDocumentDocumentLink
  | DocumentLinkResolve
  | TextDocumentRename
  | ClientRequestMisc Text
  deriving (Eq,Ord,Read,Show)
-- }}}
data ClientNotificationMethod-- {{{
  = Initialized
  | Exit
  | WorkspaceDidChangeConfiguration
  | WorkspaceDidChangeWatchedFiles
  | TextDocumentDidOpen
  | TextDocumentDidChange
  | TextDocumentWillSave
  | TextDocumentDidSave
  | TextDocumentDidClose
  | ClientCancel
  | ClientNotificationMisc Text
  deriving (Eq,Ord,Read,Show)
-- }}}

data ServerRequestMethod -- {{{
  = WindowShowMessageRequest
  | ClientRegisterCapability
  | ClientUnregisterCapability
  | WorkspaceApplyEdit
  | ServerRequestMisc Text
  deriving (Eq,Ord,Read,Show)
-- }}}
data ServerNotificationMethod -- {{{
  = WindowShowMessage
  | WindowLogMessage
  | TelemetryEvent
  | TextDocumentPublishDiagnostics
  | ServerCancel
  | ServerNotificationMisc Text
  deriving (Eq,Ord,Read,Show)
-- }}}

instance FromJSON ClientRequestMethod where -- {{{
  parseJSON (String "initialize")                       = return Initialize
  parseJSON (String "shutdown")                         = return Shutdown
  parseJSON (String "workspace/symbol")                 = return WorkspaceSymbol
  parseJSON (String "workspace/executeCommand")         = return WorkspaceExecuteCommand
  parseJSON (String "textDocument/willSaveWaitUntil")   = return TextDocumentWillSaveWaitUntil
  parseJSON (String "textDocument/completion")          = return TextDocumentCompletion
  parseJSON (String "completionItem/resolve")           = return CompletionItemResolve
  parseJSON (String "textDocument/hover")               = return TextDocumentHover
  parseJSON (String "textDocument/signatureHelp")       = return TextDocumentSignatureHelp
  parseJSON (String "textDocument/references")          = return TextDocumentReferences
  parseJSON (String "textDocument/documentHighlight")   = return TextDocumentDocumentHighlight
  parseJSON (String "textDocument/documentSymbol")      = return TextDocumentDocumentSymbol
  parseJSON (String "textDocument/formatting")          = return TextDocumentFormatting
  parseJSON (String "textDocument/rangeFormatting")     = return TextDocumentRangeFormatting
  parseJSON (String "textDocument/onTypeFormatting")    = return TextDocumentOnTypeFormatting
  parseJSON (String "textDocument/definition")          = return TextDocumentDefinition
  parseJSON (String "textDocument/codeAction")          = return TextDocumentCodeAction
  parseJSON (String "textDocument/codeLens")            = return TextDocumentCodeLens
  parseJSON (String "codeLens/resolve")                 = return CodeLensResolve
  parseJSON (String "textDocument/documentLink")        = return TextDocumentDocumentLink
  parseJSON (String "documentLink/resolve")             = return DocumentLinkResolve
  parseJSON (String "textDocument/rename")              = return TextDocumentRename
  parseJSON (String x) | "$/" `T.isPrefixOf` x          = return (ClientRequestMisc (T.drop 2 x))
  parseJSON _                                           = mempty
-- }}}
instance FromJSON ClientNotificationMethod where -- {{{
  parseJSON (String "initialized")                      = return Initialized
  parseJSON (String "exit")                             = return Exit
  parseJSON (String "$/cancelRequest")                  = return ClientCancel
  parseJSON (String "workspace/didChangeConfiguration") = return WorkspaceDidChangeConfiguration
  parseJSON (String "workspace/didChangeWatchedFiles")  = return WorkspaceDidChangeWatchedFiles
  parseJSON (String "textDocument/didOpen")             = return TextDocumentDidOpen
  parseJSON (String "textDocument/didChange")           = return TextDocumentDidChange
  parseJSON (String "textDocument/willSave")            = return TextDocumentWillSave
  parseJSON (String "textDocument/didSave")             = return TextDocumentDidSave
  parseJSON (String "textDocument/didClose")            = return TextDocumentDidClose
  parseJSON (String x) | "$/" `T.isPrefixOf` x          = return (ClientNotificationMisc (T.drop 2 x))
  parseJSON _                                           = mempty
-- }}}
instance ToJSON ClientRequestMethod where -- {{{
  toJSON Initialize                      = String "initialize"
  toJSON Shutdown                        = String "shutdown"
  toJSON WorkspaceSymbol                 = String "workspace/symbol"
  toJSON WorkspaceExecuteCommand         = String "workspace/executeCommand"
  toJSON TextDocumentWillSaveWaitUntil   = String "textDocument/willSaveWaitUntil"
  toJSON TextDocumentCompletion          = String "textDocument/completion"
  toJSON CompletionItemResolve           = String "completionItem/resolve"
  toJSON TextDocumentHover               = String "textDocument/hover"
  toJSON TextDocumentSignatureHelp       = String "textDocument/signatureHelp"
  toJSON TextDocumentReferences          = String "textDocument/references"
  toJSON TextDocumentDocumentHighlight   = String "textDocument/documentHighlight"
  toJSON TextDocumentDocumentSymbol      = String "textDocument/documentSymbol"
  toJSON TextDocumentFormatting          = String "textDocument/formatting"
  toJSON TextDocumentRangeFormatting     = String "textDocument/rangeFormatting"
  toJSON TextDocumentOnTypeFormatting    = String "textDocument/onTypeFormatting"
  toJSON TextDocumentDefinition          = String "textDocument/definition"
  toJSON TextDocumentCodeAction          = String "textDocument/codeAction"
  toJSON TextDocumentCodeLens            = String "textDocument/codeLens"
  toJSON CodeLensResolve                 = String "codeLens/resolve"
  toJSON TextDocumentRename              = String "textDocument/rename"
  toJSON TextDocumentDocumentLink        = String "textDocument/documentLink"
  toJSON DocumentLinkResolve             = String "documentLink/resolve"
  toJSON (ClientRequestMisc x)           = String ("$/" `T.append` x)
-- }}}
instance ToJSON ClientNotificationMethod where -- {{{
  toJSON Initialized                     = String "initialized"
  toJSON Exit                            = String "exit"
  toJSON ClientCancel                    = String "$/cancelRequest"
  toJSON WorkspaceDidChangeConfiguration = String "workspace/didChangeConfiguration"
  toJSON WorkspaceDidChangeWatchedFiles  = String "workspace/didChangeWatchedFiles"
  toJSON TextDocumentDidOpen             = String "textDocument/didOpen"
  toJSON TextDocumentWillSave            = String "textDocument/willSave"
  toJSON TextDocumentDidChange           = String "textDocument/didChange"
  toJSON TextDocumentDidSave             = String "textDocument/didSave"
  toJSON TextDocumentDidClose            = String "textDocument/didClose"
  toJSON (ClientNotificationMisc x)      = String ("$/" `T.append` x)
-- }}}

instance FromJSON ServerRequestMethod where -- {{{
  parseJSON (String "window/showMessageRequest")       = return WindowShowMessageRequest
  parseJSON (String "client/registerCapability")       = return ClientRegisterCapability
  parseJSON (String "client/unregisterCapability")     = return ClientUnregisterCapability
  parseJSON (String "workspace/applyEdit")             = return WorkspaceApplyEdit
  parseJSON (String x) | "$/" `T.isPrefixOf` x         = return (ServerRequestMisc (T.drop 2 x))
  parseJSON _                                          = mempty
-- }}}
instance FromJSON ServerNotificationMethod where -- {{{
  parseJSON (String "window/showMessage")              = return WindowShowMessage
  parseJSON (String "window/logMessage")               = return WindowLogMessage
  parseJSON (String "telemetry/event")                 = return TelemetryEvent
  parseJSON (String "textDocument/publishDiagnostics") = return TextDocumentPublishDiagnostics
  parseJSON (String x) | "$/" `T.isPrefixOf` x         = return (ServerNotificationMisc (T.drop 2 x))
  parseJSON _                                          = mempty
-- }}}
instance ToJSON ServerRequestMethod where -- {{{
  toJSON WindowShowMessageRequest   = String "window/showMessageRequest"
  toJSON ClientRegisterCapability   = String "client/registerCapability"
  toJSON ClientUnregisterCapability = String "client/unregisterCapability"
  toJSON WorkspaceApplyEdit         = String "workspace/applyEdit"
  toJSON (ServerRequestMisc x)      = String ("$/" `T.append` x)
-- }}}
instance ToJSON ServerNotificationMethod where -- {{{
  toJSON WindowShowMessage              = String "window/showMessage"
  toJSON WindowLogMessage               = String "window/logMessage"
  toJSON TelemetryEvent                 = String "telemetry/event"
  toJSON TextDocumentPublishDiagnostics = String "textDocument/publishDiagnostics"
  toJSON ServerCancel                   = String "$/cancel$"
  toJSON (ServerNotificationMisc x)     = String ("$/" `T.append` x)
-- }}}

-------------------------------------------------------------------------------
-- Method Kind
-------------------------------------------------------------------------------

data ClientRequestMethodK --{{{
  = InitializeK
  | ShutdownK
  | WorkspaceSymbolK
  | WorkspaceExecuteCommandK
  | TextDocumentWillSaveWaitUntilK
  | TextDocumentCompletionK
  | CompletionItemResolveK
  | TextDocumentHoverK
  | TextDocumentSignatureHelpK
  | TextDocumentReferencesK
  | TextDocumentDocumentHighlightK
  | TextDocumentDocumentSymbolK
  | TextDocumentFormattingK
  | TextDocumentRangeFormattingK
  | TextDocumentOnTypeFormattingK
  | TextDocumentDefinitionK
  | TextDocumentCodeActionK
  | TextDocumentCodeLensK
  | CodeLensResolveK
  | TextDocumentDocumentLinkK
  | DocumentLinkResolveK
  | TextDocumentRenameK
  | ClientRequestMiscK Symbol
-- }}}
data ClientNotificationMethodK --{{{
  = InitializedK
  | ClientCancelK
  | ExitK
  | WorkspaceDidChangeConfigurationK
  | WorkspaceDidChangeWatchedFilesK
  | TextDocumentDidOpenK
  | TextDocumentDidChangeK
  | TextDocumentWillSaveK
  | TextDocumentDidSaveK
  | TextDocumentDidCloseK
  | ClientNotificationMiscK Symbol
-- }}}

data ServerRequestMethodK -- {{{
  = WindowShowMessageRequestK
  | ClientRegisterCapabilityK
  | ClientUnregisterCapabilityK
  | WorkspaceApplyEditK
  | ServerRequestMiscK Symbol
-- }}}
data ServerNotificationMethodK -- {{{
  = WindowShowMessageK
  | WindowLogMessageK
  | TelemetryEventK
  | TextDocumentPublishDiagnosticsK
  | ServerCancelK
  | ServerNotificationMiscK Symbol
-- }}}

-------------------------------------------------------------------------------
-- Singletons
-------------------------------------------------------------------------------

-- Client
---------

-- data Sing{{{
data instance Sing (m :: ClientRequestMethodK) where -- {{{
  SInitialize                      :: Sing 'InitializeK
  SShutdown                        :: Sing 'ShutdownK
  SWorkspaceSymbol                 :: Sing 'WorkspaceSymbolK
  SWorkspaceExecuteCommand         :: Sing 'WorkspaceExecuteCommandK
  STextDocumentWillSaveWaitUntil   :: Sing 'TextDocumentWillSaveWaitUntilK
  STextDocumentCompletion          :: Sing 'TextDocumentCompletionK
  SCompletionItemResolve           :: Sing 'CompletionItemResolveK
  STextDocumentHover               :: Sing 'TextDocumentHoverK
  STextDocumentSignatureHelp       :: Sing 'TextDocumentSignatureHelpK
  STextDocumentReferences          :: Sing 'TextDocumentReferencesK
  STextDocumentDocumentHighlight   :: Sing 'TextDocumentDocumentHighlightK
  STextDocumentDocumentSymbol      :: Sing 'TextDocumentDocumentSymbolK
  STextDocumentFormatting          :: Sing 'TextDocumentFormattingK
  STextDocumentRangeFormatting     :: Sing 'TextDocumentRangeFormattingK
  STextDocumentOnTypeFormatting    :: Sing 'TextDocumentOnTypeFormattingK
  STextDocumentDefinition          :: Sing 'TextDocumentDefinitionK
  STextDocumentCodeAction          :: Sing 'TextDocumentCodeActionK
  STextDocumentCodeLens            :: Sing 'TextDocumentCodeLensK
  SCodeLensResolve                 :: Sing 'CodeLensResolveK
  STextDocumentDocumentLink        :: Sing 'TextDocumentDocumentLinkK
  SDocumentLinkResolve             :: Sing 'DocumentLinkResolveK
  STextDocumentRename              :: Sing 'TextDocumentRenameK
  SClientRequestMisc               :: Sing n -> Sing ('ClientRequestMiscK n)
-- }}}
data instance Sing (m :: ClientNotificationMethodK) where -- {{{
  SInitialized                     :: Sing 'InitializedK
  SExit                            :: Sing 'ExitK
  SClientCancel                    :: Sing ' ClientCancelK
  SWorkspaceDidChangeConfiguration :: Sing 'WorkspaceDidChangeConfigurationK
  SWorkspaceDidChangeWatchedFiles  :: Sing 'WorkspaceDidChangeWatchedFilesK
  STextDocumentDidOpen             :: Sing 'TextDocumentDidOpenK
  STextDocumentDidChange           :: Sing 'TextDocumentDidChangeK
  STextDocumentWillSave            :: Sing 'TextDocumentWillSaveK
  STextDocumentDidSave             :: Sing 'TextDocumentDidSaveK
  STextDocumentDidClose            :: Sing 'TextDocumentDidCloseK
  SClientNotificationMisc          :: Sing n -> Sing ('ClientNotificationMiscK n)
--}}}
-- }}}
-- instance SingI  -- {{{
instance SingI 'InitializeK                      where sing = SInitialize
instance SingI 'InitializedK                     where sing = SInitialized
instance SingI 'ShutdownK                        where sing = SShutdown
instance SingI 'ExitK                            where sing = SExit
instance SingI 'ClientCancelK                    where sing = SClientCancel
instance SingI 'WorkspaceDidChangeConfigurationK where sing = SWorkspaceDidChangeConfiguration
instance SingI 'WorkspaceDidChangeWatchedFilesK  where sing = SWorkspaceDidChangeWatchedFiles
instance SingI 'WorkspaceSymbolK                 where sing = SWorkspaceSymbol
instance SingI 'WorkspaceExecuteCommandK         where sing = SWorkspaceExecuteCommand
instance SingI 'TextDocumentDidOpenK             where sing = STextDocumentDidOpen
instance SingI 'TextDocumentDidChangeK           where sing = STextDocumentDidChange
instance SingI 'TextDocumentWillSaveK            where sing = STextDocumentWillSave
instance SingI 'TextDocumentWillSaveWaitUntilK   where sing = STextDocumentWillSaveWaitUntil
instance SingI 'TextDocumentDidSaveK             where sing = STextDocumentDidSave
instance SingI 'TextDocumentDidCloseK            where sing = STextDocumentDidClose
instance SingI 'TextDocumentCompletionK          where sing = STextDocumentCompletion
instance SingI 'CompletionItemResolveK           where sing = SCompletionItemResolve
instance SingI 'TextDocumentHoverK               where sing = STextDocumentHover
instance SingI 'TextDocumentSignatureHelpK       where sing = STextDocumentSignatureHelp
instance SingI 'TextDocumentReferencesK          where sing = STextDocumentReferences
instance SingI 'TextDocumentDocumentHighlightK   where sing = STextDocumentDocumentHighlight
instance SingI 'TextDocumentDocumentSymbolK      where sing = STextDocumentDocumentSymbol
instance SingI 'TextDocumentFormattingK          where sing = STextDocumentFormatting
instance SingI 'TextDocumentRangeFormattingK     where sing = STextDocumentRangeFormatting
instance SingI 'TextDocumentOnTypeFormattingK    where sing = STextDocumentOnTypeFormatting
instance SingI 'TextDocumentDefinitionK          where sing = STextDocumentDefinition
instance SingI 'TextDocumentCodeActionK          where sing = STextDocumentCodeAction
instance SingI 'TextDocumentCodeLensK            where sing = STextDocumentCodeLens
instance SingI 'CodeLensResolveK                 where sing = SCodeLensResolve
instance SingI 'TextDocumentDocumentLinkK        where sing = STextDocumentDocumentLink
instance SingI 'DocumentLinkResolveK             where sing = SDocumentLinkResolve
instance SingI 'TextDocumentRenameK              where sing = STextDocumentRename
instance KnownSymbol n => SingI ('ClientRequestMiscK n) where sing = SClientRequestMisc sing
instance KnownSymbol n => SingI ('ClientNotificationMiscK n) where sing = SClientNotificationMisc sing
-- }}}
-- instance SingKind -- {{{
instance SingKind ClientRequestMethodK where-- {{{
  type Demote ClientRequestMethodK = ClientRequestMethod
  fromSing = \case --{{{
    SInitialize                      -> Initialize
    SShutdown                        -> Shutdown
    SWorkspaceSymbol                 -> WorkspaceSymbol
    SWorkspaceExecuteCommand         -> WorkspaceExecuteCommand
    STextDocumentWillSaveWaitUntil   -> TextDocumentWillSaveWaitUntil
    STextDocumentCompletion          -> TextDocumentCompletion
    SCompletionItemResolve           -> CompletionItemResolve
    STextDocumentHover               -> TextDocumentHover
    STextDocumentSignatureHelp       -> TextDocumentSignatureHelp
    STextDocumentReferences          -> TextDocumentReferences
    STextDocumentDocumentHighlight   -> TextDocumentDocumentHighlight
    STextDocumentDocumentSymbol      -> TextDocumentDocumentSymbol
    STextDocumentFormatting          -> TextDocumentFormatting
    STextDocumentRangeFormatting     -> TextDocumentRangeFormatting
    STextDocumentOnTypeFormatting    -> TextDocumentOnTypeFormatting
    STextDocumentDefinition          -> TextDocumentDefinition
    STextDocumentCodeAction          -> TextDocumentCodeAction
    STextDocumentCodeLens            -> TextDocumentCodeLens
    SCodeLensResolve                 -> CodeLensResolve
    STextDocumentDocumentLink        -> TextDocumentDocumentLink
    SDocumentLinkResolve             -> DocumentLinkResolve
    STextDocumentRename              -> TextDocumentRename
    SClientRequestMisc s             -> ClientRequestMisc (fromSing s)
   --}}}
  toSing = \case --{{{
    Initialize                         -> SomeSing SInitialize
    Shutdown                           -> SomeSing SShutdown
    WorkspaceSymbol                    -> SomeSing SWorkspaceSymbol
    WorkspaceExecuteCommand            -> SomeSing SWorkspaceExecuteCommand
    TextDocumentWillSaveWaitUntil      -> SomeSing STextDocumentWillSaveWaitUntil
    TextDocumentCompletion             -> SomeSing STextDocumentCompletion
    CompletionItemResolve              -> SomeSing SCompletionItemResolve
    TextDocumentHover                  -> SomeSing STextDocumentHover
    TextDocumentSignatureHelp          -> SomeSing STextDocumentSignatureHelp
    TextDocumentReferences             -> SomeSing STextDocumentReferences
    TextDocumentDocumentHighlight      -> SomeSing STextDocumentDocumentHighlight
    TextDocumentDocumentSymbol         -> SomeSing STextDocumentDocumentSymbol
    TextDocumentFormatting             -> SomeSing STextDocumentFormatting
    TextDocumentRangeFormatting        -> SomeSing STextDocumentRangeFormatting
    TextDocumentOnTypeFormatting       -> SomeSing STextDocumentOnTypeFormatting
    TextDocumentDefinition             -> SomeSing STextDocumentDefinition
    TextDocumentCodeAction             -> SomeSing STextDocumentCodeAction
    TextDocumentCodeLens               -> SomeSing STextDocumentCodeLens
    CodeLensResolve                    -> SomeSing SCodeLensResolve
    TextDocumentDocumentLink           -> SomeSing STextDocumentDocumentLink
    DocumentLinkResolve                -> SomeSing SDocumentLinkResolve
    TextDocumentRename                 -> SomeSing STextDocumentRename
    ClientRequestMisc n                -> case toSing n of
                                            SomeSing s -> SomeSing (SClientRequestMisc s)
    -- }}}
-- }}}
instance SingKind ClientNotificationMethodK where -- {{{
  type Demote ClientNotificationMethodK = ClientNotificationMethod
  fromSing = \case --{{{
    SInitialized                     -> Initialized
    SExit                            -> Exit
    SClientCancel                    -> ClientCancel
    SWorkspaceDidChangeConfiguration -> WorkspaceDidChangeConfiguration
    SWorkspaceDidChangeWatchedFiles  -> WorkspaceDidChangeWatchedFiles
    STextDocumentDidOpen             -> TextDocumentDidOpen
    STextDocumentDidChange           -> TextDocumentDidChange
    STextDocumentWillSave            -> TextDocumentWillSave
    STextDocumentDidSave             -> TextDocumentDidSave
    STextDocumentDidClose            -> TextDocumentDidClose
    SClientNotificationMisc s        -> ClientNotificationMisc (fromSing s)
    -- }}}
  toSing = \case --{{{
    Initialized                        -> SomeSing SInitialized
    Exit                               -> SomeSing SExit
    ClientCancel                       -> SomeSing SClientCancel
    WorkspaceDidChangeConfiguration    -> SomeSing SWorkspaceDidChangeConfiguration
    WorkspaceDidChangeWatchedFiles     -> SomeSing SWorkspaceDidChangeWatchedFiles
    TextDocumentDidOpen                -> SomeSing STextDocumentDidOpen
    TextDocumentDidChange              -> SomeSing STextDocumentDidChange
    TextDocumentWillSave               -> SomeSing STextDocumentWillSave
    TextDocumentDidSave                -> SomeSing STextDocumentDidSave
    TextDocumentDidClose               -> SomeSing STextDocumentDidClose
    ClientNotificationMisc n           -> case toSing n of
                                            SomeSing s -> SomeSing (SClientNotificationMisc s)
    -- }}}
-- }}}
-- }}}

-- Server
---------

-- data Sing{{{
data instance Sing (m :: ServerRequestMethodK) where -- {{{
  SWindowShowMessageRequest       :: Sing 'WindowShowMessageRequestK
  SClientRegisterCapability       :: Sing 'ClientRegisterCapabilityK
  SWorkspaceApplyEdit             :: Sing 'WorkspaceApplyEditK
  SClientUnregisterCapability     :: Sing 'ClientUnregisterCapabilityK
  SServerRequestMisc              :: Sing n -> Sing ('ServerRequestMiscK n)
-- }}}
data instance Sing (m :: ServerNotificationMethodK) where -- {{{
  SWindowShowMessage              :: Sing 'WindowShowMessageK
  SWindowLogMessage               :: Sing 'WindowLogMessageK
  STelemetryEvent                 :: Sing 'TelemetryEventK
  STextDocumentPublishDiagnostics :: Sing 'TextDocumentPublishDiagnosticsK
  SServerCancel                   :: Sing 'ServerCancelK
  SServerNotificationMisc         :: Sing n -> Sing ('ServerNotificationMiscK n)
-- }}}
-- }}}
-- instance SingI  --{{{
instance SingI 'WindowShowMessageK              where sing = SWindowShowMessage
instance SingI 'WindowShowMessageRequestK       where sing = SWindowShowMessageRequest
instance SingI 'WindowLogMessageK               where sing = SWindowLogMessage
instance SingI 'TelemetryEventK                 where sing = STelemetryEvent
instance SingI 'ClientRegisterCapabilityK       where sing = SClientRegisterCapability
instance SingI 'ClientUnregisterCapabilityK     where sing = SClientUnregisterCapability
instance SingI 'WorkspaceApplyEditK             where sing = SWorkspaceApplyEdit
instance SingI 'TextDocumentPublishDiagnosticsK where sing = STextDocumentPublishDiagnostics
instance KnownSymbol n => SingI ('ServerRequestMiscK n) where sing = SServerRequestMisc sing
instance KnownSymbol n => SingI ('ServerNotificationMiscK n) where sing = SServerNotificationMisc sing
-- }}}
-- instance SingKind {{{
instance SingKind ServerRequestMethodK where -- {{{
  type Demote ServerRequestMethodK = ServerRequestMethod
  fromSing = \case --{{{
    SWindowShowMessageRequest       -> WindowShowMessageRequest
    SClientRegisterCapability       -> ClientRegisterCapability
    SClientUnregisterCapability     -> ClientUnregisterCapability
    SWorkspaceApplyEdit             -> WorkspaceApplyEdit
    SServerRequestMisc s            -> ServerRequestMisc (fromSing s)
  -- }}}
  toSing = \case --{{{
    WindowShowMessageRequest       -> SomeSing SWindowShowMessageRequest
    ClientRegisterCapability       -> SomeSing SClientRegisterCapability
    ClientUnregisterCapability     -> SomeSing SClientUnregisterCapability
    WorkspaceApplyEdit             -> SomeSing SWorkspaceApplyEdit
    ServerRequestMisc n            -> case toSing n of
                                        SomeSing s -> SomeSing (SServerRequestMisc s)
  -- }}}
  -- }}}
instance SingKind ServerNotificationMethodK where -- {{{
  type Demote ServerNotificationMethodK = ServerNotificationMethod
  fromSing = \case --{{{
    SWindowShowMessage              -> WindowShowMessage
    SWindowLogMessage               -> WindowLogMessage
    STelemetryEvent                 -> TelemetryEvent
    STextDocumentPublishDiagnostics -> TextDocumentPublishDiagnostics
    SServerCancel                   -> ServerCancel
    SServerNotificationMisc s       -> ServerNotificationMisc (fromSing s)
  -- }}}
  toSing = \case --{{{
    WindowShowMessage              -> SomeSing SWindowShowMessage
    WindowLogMessage               -> SomeSing SWindowLogMessage
    TelemetryEvent                 -> SomeSing STelemetryEvent
    TextDocumentPublishDiagnostics -> SomeSing STextDocumentPublishDiagnostics
    ServerCancel                   -> SomeSing SServerCancel
    ServerNotificationMisc n       -> case toSing n of
                                        SomeSing s -> SomeSing (SServerNotificationMisc s)
  -- }}}
  -- }}}
-- }}}

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class (SingKind k, Eq (Demote k), Show (Demote k), FieldJSON (Demote k)) => IsMethodKind k
instance IsMethodKind ClientRequestMethodK
instance IsMethodKind ClientNotificationMethodK
instance IsMethodKind ServerRequestMethodK
instance IsMethodKind ServerNotificationMethodK

