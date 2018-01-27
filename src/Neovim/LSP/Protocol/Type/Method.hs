
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleContexts        #-}
-- the following two are only for `Bottom`
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wall                #-}

module Neovim.LSP.Protocol.Type.Method
  ( ClientMethod(..)
  , ClientMethodK(..)
  , ServerMethod(..)
  , ServerMethodK(..)
  , X(..)
  -- Constraint
  , IsClientRequest
  , IsClientNotification
  , IsServerRequest
  , IsServerNotification
  , Sing(..)

  , Method
  , Actor
  , IsMethodKind
  -- Constraint util
  , Top, Bottom, bottom
  )
  where

import           Data.Aeson
import           Data.Constraint          hiding (Bottom, bottom)
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import qualified Data.Text                as T
import qualified GHC.Exts                 (Any)
import           Neovim.LSP.Protocol.Type.JSON

-------------------------------------------------------------------------------
-- Method
-------------------------------------------------------------------------------

data ClientMethod -- {{{
  -- General
  = Initialize                      -- Req
  | Initialized                     -- Noti
  | Shutdown                        -- Req
  | Exit                            -- Noti
  | CancelRequest                   -- Noti -- TODO serverも使う
  -- Workspace
  | WorkspaceDidChangeConfiguration -- Noti
  | WorkspaceDidChangeWatchedFiles  -- Noti
  | WorkspaceSymbol                 -- Req
  | WorkspaceExecuteCommand         -- Req
  -- Document
  | TextDocumentDidOpen             -- Noti
  | TextDocumentDidChange           -- Noti
  | TextDocumentWillSave            -- Noti
  | TextDocumentWillSaveWaitUntil   -- Req
  | TextDocumentDidSave             -- Noti
  | TextDocumentDidClose            -- Noti
  | TextDocumentCompletion          -- Req
  | CompletionItemResolve           -- Req
  | TextDocumentHover               -- Req
  | TextDocumentSignatureHelp       -- Req
  | TextDocumentReferences          -- Req
  | TextDocumentDocumentHighlight   -- Req
  | TextDocumentDocumentSymbol      -- Req
  | TextDocumentFormatting          -- Req
  | TextDocumentRangeFormatting     -- Req
  | TextDocumentOnTypeFormatting    -- Req
  | TextDocumentDefinition          -- Req
  | TextDocumentCodeAction          -- Req
  | TextDocumentCodeLens            -- Req
  | CodeLensResolve                 -- Req
  | TextDocumentDocumentLink        -- Req
  | DocumentLinkResolve             -- Req
  | TextDocumentRename              -- Req
  -- Messages of the form $/message
  -- Implementation Dependent, can be ignored
  | Misc T.Text                     -- Both
  deriving (Eq,Ord,Read,Show)
-- }}}
data ServerMethod -- {{{
  -- Window
  = WindowShowMessage               -- Noti
  | WindowShowMessageRequest        -- Req
  | WindowLogMessage                -- Noti
  | TelemetryEvent                  -- Noti
  -- Client
  | ClientRegisterCapability        -- Req
  | ClientUnregisterCapability      -- Req
  -- Workspace
  | WorkspaceApplyEdit              -- Req
  -- Document
  | TextDocumentPublishDiagnostics  -- Noti
  -- ServerCancelRequest             -- Noti -- TODO
  deriving (Eq,Ord,Read,Show)
-- }}}

instance FromJSON ClientMethod where -- {{{
  -- General
  parseJSON (String "initialize")                       = return Initialize
  parseJSON (String "initialized")                      = return Initialized
  parseJSON (String "shutdown")                         = return Shutdown
  parseJSON (String "exit")                             = return Exit
  parseJSON (String "$/cancelRequest")                  = return CancelRequest
 -- Workspace
  parseJSON (String "workspace/didChangeConfiguration") = return WorkspaceDidChangeConfiguration
  parseJSON (String "workspace/didChangeWatchedFiles")  = return WorkspaceDidChangeWatchedFiles
  parseJSON (String "workspace/symbol")                 = return WorkspaceSymbol
  parseJSON (String "workspace/executeCommand")         = return WorkspaceExecuteCommand
 -- Document
  parseJSON (String "textDocument/didOpen")             = return TextDocumentDidOpen
  parseJSON (String "textDocument/didChange")           = return TextDocumentDidChange
  parseJSON (String "textDocument/willSave")            = return TextDocumentWillSave
  parseJSON (String "textDocument/willSaveWaitUntil")   = return TextDocumentWillSaveWaitUntil
  parseJSON (String "textDocument/didSave")             = return TextDocumentDidSave
  parseJSON (String "textDocument/didClose")            = return TextDocumentDidClose
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
  parseJSON (String x) | "$/" `T.isPrefixOf` x          = return (Misc (T.drop 2 x))
  parseJSON _                                           = mempty
-- }}}
instance ToJSON ClientMethod where -- {{{
  -- General
  toJSON Initialize                      = String "initialize"
  toJSON Initialized                     = String "initialized"
  toJSON Shutdown                        = String "shutdown"
  toJSON Exit                            = String "exit"
  toJSON CancelRequest                   = String "$/cancelRequest"
  -- Workspace
  toJSON WorkspaceDidChangeConfiguration = String "workspace/didChangeConfiguration"
  toJSON WorkspaceDidChangeWatchedFiles  = String "workspace/didChangeWatchedFiles"
  toJSON WorkspaceSymbol                 = String "workspace/symbol"
  toJSON WorkspaceExecuteCommand         = String "workspace/executeCommand"
  -- Document
  toJSON TextDocumentDidOpen             = String "textDocument/didOpen"
  toJSON TextDocumentDidChange           = String "textDocument/didChange"
  toJSON TextDocumentWillSave            = String "textDocument/willSave"
  toJSON TextDocumentWillSaveWaitUntil   = String "textDocument/willSaveWaitUntil"
  toJSON TextDocumentDidSave             = String "textDocument/didSave"
  toJSON TextDocumentDidClose            = String "textDocument/didClose"
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
  toJSON (Misc x)                        = String ("$/" `T.append` x)
-- }}}

instance FromJSON ServerMethod where -- {{{
  -- Window
  parseJSON (String "window/showMessage")              = return WindowShowMessage
  parseJSON (String "window/showMessageRequest")       = return WindowShowMessageRequest
  parseJSON (String "window/logMessage")               = return WindowLogMessage
  parseJSON (String "telemetry/event")                 = return TelemetryEvent
  -- Client
  parseJSON (String "client/registerCapability")       = return ClientRegisterCapability
  parseJSON (String "client/unregisterCapability")     = return ClientUnregisterCapability
  -- Workspace
  parseJSON (String "workspace/applyEdit")             = return WorkspaceApplyEdit
  -- Document
  parseJSON (String "textDocument/publishDiagnostics") = return TextDocumentPublishDiagnostics
  parseJSON _                                          = mempty
-- }}}
instance ToJSON ServerMethod where -- {{{
  -- Window
  toJSON WindowShowMessage = String "window/showMessage"
  toJSON WindowShowMessageRequest = String "window/showMessageRequest"
  toJSON WindowLogMessage = String "window/logMessage"
  toJSON TelemetryEvent = String "telemetry/event"
  -- Client
  toJSON ClientRegisterCapability = String "client/registerCapability"
  toJSON ClientUnregisterCapability = String "client/unregisterCapability"
  -- Workspace
  toJSON WorkspaceApplyEdit = String "workspace/applyEdit"
  -- Document
  toJSON TextDocumentPublishDiagnostics = String "textDocument/publishDiagnostics"
-- }}}

-------------------------------------------------------------------------------
-- Method Kind
-------------------------------------------------------------------------------

data ClientMethodK -- {{{
  -- General
  = InitializeK                      -- Req
  | InitializedK                     -- Noti
  | ShutdownK                        -- Req
  | ExitK                            -- Noti
  | CancelRequestK                   -- Req
  -- Workspace
  | WorkspaceDidChangeConfigurationK -- Noti
  | WorkspaceDidChangeWatchedFilesK  -- Noti
  | WorkspaceSymbolK                 -- Req
  | WorkspaceExecuteCommandK         -- Req
  -- Document
  | TextDocumentDidOpenK             -- Noti
  | TextDocumentDidChangeK           -- Noti
  | TextDocumentWillSaveK            -- Noti
  | TextDocumentWillSaveWaitUntilK   -- Req
  | TextDocumentDidSaveK             -- Noti
  | TextDocumentDidCloseK            -- Noti
  | TextDocumentCompletionK          -- Req
  | CompletionItemResolveK           -- Req
  | TextDocumentHoverK               -- Req
  | TextDocumentSignatureHelpK       -- Req
  | TextDocumentReferencesK          -- Req
  | TextDocumentDocumentHighlightK   -- Req
  | TextDocumentDocumentSymbolK      -- Req
  | TextDocumentFormattingK          -- Req
  | TextDocumentRangeFormattingK     -- Req
  | TextDocumentOnTypeFormattingK    -- Req
  | TextDocumentDefinitionK          -- Req
  | TextDocumentCodeActionK          -- Req
  | TextDocumentCodeLensK            -- Req
  | CodeLensResolveK                 -- Req
  | TextDocumentDocumentLinkK        -- Req
  | DocumentLinkResolveK             -- Req
  | TextDocumentRenameK              -- Req
  | MiscK Symbol                     -- Both
-- }}}
data ServerMethodK -- {{{
  -- Window
  = WindowShowMessageK              -- Noti
  | WindowShowMessageRequestK       -- Req
  | WindowLogMessageK               -- Noti
  | TelemetryEventK                 -- Noti
  -- Client
  | ClientRegisterCapabilityK       -- Req
  | ClientUnregisterCapabilityK     -- Req
  -- Workspace
  | WorkspaceApplyEditK             -- Req
  -- Document
  | TextDocumentPublishDiagnosticsK -- Noti
  -- ServerCancelRequest
  deriving (Eq,Ord,Read,Show)
-- }}}

-- type definition
type family IsClientRequest (m :: ClientMethodK) :: Constraint where-- {{{
  IsClientRequest 'InitializeK                      = Top
  IsClientRequest 'ShutdownK                        = Top
  IsClientRequest 'WorkspaceSymbolK                 = Top
  IsClientRequest 'WorkspaceExecuteCommandK         = Top
  IsClientRequest 'TextDocumentWillSaveWaitUntilK   = Top
  IsClientRequest 'TextDocumentCompletionK          = Top
  IsClientRequest 'CompletionItemResolveK           = Top
  IsClientRequest 'TextDocumentHoverK               = Top
  IsClientRequest 'TextDocumentSignatureHelpK       = Top
  IsClientRequest 'TextDocumentReferencesK          = Top
  IsClientRequest 'TextDocumentDocumentHighlightK   = Top
  IsClientRequest 'TextDocumentDocumentSymbolK      = Top
  IsClientRequest 'TextDocumentFormattingK          = Top
  IsClientRequest 'TextDocumentRangeFormattingK     = Top
  IsClientRequest 'TextDocumentOnTypeFormattingK    = Top
  IsClientRequest 'TextDocumentDefinitionK          = Top
  IsClientRequest 'TextDocumentCodeActionK          = Top
  IsClientRequest 'TextDocumentCodeLensK            = Top
  IsClientRequest 'CodeLensResolveK                 = Top
  IsClientRequest 'TextDocumentDocumentLinkK        = Top
  IsClientRequest 'DocumentLinkResolveK             = Top
  IsClientRequest 'TextDocumentRenameK              = Top
  IsClientRequest ('MiscK s)                        = Top
  IsClientRequest _                                 = Bottom
-- }}}
type family IsClientNotification (m :: ClientMethodK) :: Constraint where-- {{{
  IsClientNotification 'InitializedK                     = Top
  IsClientNotification 'CancelRequestK                   = Top
  IsClientNotification 'ExitK                            = Top
  IsClientNotification 'WorkspaceDidChangeConfigurationK = Top
  IsClientNotification 'WorkspaceDidChangeWatchedFilesK  = Top
  IsClientNotification 'TextDocumentDidOpenK             = Top
  IsClientNotification 'TextDocumentDidChangeK           = Top
  IsClientNotification 'TextDocumentWillSaveK            = Top
  IsClientNotification 'TextDocumentDidSaveK             = Top
  IsClientNotification 'TextDocumentDidCloseK            = Top
  IsClientNotification _                                 = Bottom
-- }}}
type family IsServerRequest (m :: ServerMethodK) :: Constraint where-- {{{
  IsServerRequest 'WindowShowMessageRequestK   = Top
  IsServerRequest 'ClientRegisterCapabilityK   = Top
  IsServerRequest 'ClientUnregisterCapabilityK = Top
  IsServerRequest 'WorkspaceApplyEditK         = Top
  IsServerRequest _                            = Bottom
-- }}}
type family IsServerNotification (m :: ServerMethodK) :: Constraint where-- {{{
  IsServerNotification 'WindowShowMessageK              = Top
  IsServerNotification 'WindowLogMessageK               = Top
  IsServerNotification 'TelemetryEventK                 = Top
  IsServerNotification 'TextDocumentPublishDiagnosticsK = Top
  IsServerNotification _                                = Bottom
-- }}}

-------------------------------------------------------------------------------
-- Singletons
-------------------------------------------------------------------------------

-- Client
---------

data instance Sing (m ::ClientMethodK) where -- {{{
  SInitialize                      :: Sing 'InitializeK
  SInitialized                     :: Sing 'InitializedK
  SShutdown                        :: Sing 'ShutdownK
  SExit                            :: Sing 'ExitK
  SCancelRequest                   :: Sing 'CancelRequestK
  SWorkspaceDidChangeConfiguration :: Sing 'WorkspaceDidChangeConfigurationK
  SWorkspaceDidChangeWatchedFiles  :: Sing 'WorkspaceDidChangeWatchedFilesK
  SWorkspaceSymbol                 :: Sing 'WorkspaceSymbolK
  SWorkspaceExecuteCommand         :: Sing 'WorkspaceExecuteCommandK
  STextDocumentDidOpen             :: Sing 'TextDocumentDidOpenK
  STextDocumentDidChange           :: Sing 'TextDocumentDidChangeK
  STextDocumentWillSave            :: Sing 'TextDocumentWillSaveK
  STextDocumentWillSaveWaitUntil   :: Sing 'TextDocumentWillSaveWaitUntilK
  STextDocumentDidSave             :: Sing 'TextDocumentDidSaveK
  STextDocumentDidClose            :: Sing 'TextDocumentDidCloseK
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
  SMisc                            :: Sing n -> Sing ('MiscK n)
-- }}}
instance SingI 'InitializeK                      where sing = SInitialize -- {{{
instance SingI 'InitializedK                     where sing = SInitialized
instance SingI 'ShutdownK                        where sing = SShutdown
instance SingI 'ExitK                            where sing = SExit
instance SingI 'CancelRequestK                   where sing = SCancelRequest
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
instance KnownSymbol n => SingI ('MiscK n)       where sing = SMisc sing
-- }}}
instance SingKind ClientMethodK where -- {{{
  type Demote ClientMethodK = ClientMethod
  fromSing = \case --{{{
    SInitialize                      -> Initialize
    SInitialized                     -> Initialized
    SShutdown                        -> Shutdown
    SExit                            -> Exit
    SCancelRequest                   -> CancelRequest
    SWorkspaceDidChangeConfiguration -> WorkspaceDidChangeConfiguration
    SWorkspaceDidChangeWatchedFiles  -> WorkspaceDidChangeWatchedFiles
    SWorkspaceSymbol                 -> WorkspaceSymbol
    SWorkspaceExecuteCommand         -> WorkspaceExecuteCommand
    STextDocumentDidOpen             -> TextDocumentDidOpen
    STextDocumentDidChange           -> TextDocumentDidChange
    STextDocumentWillSave            -> TextDocumentWillSave
    STextDocumentWillSaveWaitUntil   -> TextDocumentWillSaveWaitUntil
    STextDocumentDidSave             -> TextDocumentDidSave
    STextDocumentDidClose            -> TextDocumentDidClose
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
    SMisc s                          -> Misc (fromSing s)
    --}}}
  toSing = \case --{{{
    Initialize                         -> SomeSing SInitialize
    Initialized                        -> SomeSing SInitialized
    Shutdown                           -> SomeSing SShutdown
    Exit                               -> SomeSing SExit
    CancelRequest                      -> SomeSing SCancelRequest
    WorkspaceDidChangeConfiguration    -> SomeSing SWorkspaceDidChangeConfiguration
    WorkspaceDidChangeWatchedFiles     -> SomeSing SWorkspaceDidChangeWatchedFiles
    WorkspaceSymbol                    -> SomeSing SWorkspaceSymbol
    WorkspaceExecuteCommand            -> SomeSing SWorkspaceExecuteCommand
    TextDocumentDidOpen                -> SomeSing STextDocumentDidOpen
    TextDocumentDidChange              -> SomeSing STextDocumentDidChange
    TextDocumentWillSave               -> SomeSing STextDocumentWillSave
    TextDocumentWillSaveWaitUntil      -> SomeSing STextDocumentWillSaveWaitUntil
    TextDocumentDidSave                -> SomeSing STextDocumentDidSave
    TextDocumentDidClose               -> SomeSing STextDocumentDidClose
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
    Misc n                             -> case toSing n of SomeSing s -> SomeSing (SMisc s)
    --}}}
-- }}}

-- Server
---------

data instance Sing (m ::ServerMethodK) where -- {{{
  SWindowShowMessage              :: Sing 'WindowShowMessageK
  SWindowShowMessageRequest       :: Sing 'WindowShowMessageRequestK
  SWindowLogMessage               :: Sing 'WindowLogMessageK
  STelemetryEvent                 :: Sing 'TelemetryEventK
  SClientRegisterCapability       :: Sing 'ClientRegisterCapabilityK
  SClientUnregisterCapability     :: Sing 'ClientUnregisterCapabilityK
  SWorkspaceApplyEdit             :: Sing 'WorkspaceApplyEditK
  STextDocumentPublishDiagnostics :: Sing 'TextDocumentPublishDiagnosticsK
-- }}}
instance SingI 'WindowShowMessageK              where sing = SWindowShowMessage --{{{
instance SingI 'WindowShowMessageRequestK       where sing = SWindowShowMessageRequest
instance SingI 'WindowLogMessageK               where sing = SWindowLogMessage
instance SingI 'TelemetryEventK                 where sing = STelemetryEvent
instance SingI 'ClientRegisterCapabilityK       where sing = SClientRegisterCapability
instance SingI 'ClientUnregisterCapabilityK     where sing = SClientUnregisterCapability
instance SingI 'WorkspaceApplyEditK             where sing = SWorkspaceApplyEdit
instance SingI 'TextDocumentPublishDiagnosticsK where sing = STextDocumentPublishDiagnostics
-- }}}
instance SingKind ServerMethodK where -- {{{
  type Demote ServerMethodK = ServerMethod
  fromSing = \case --{{{
    SWindowShowMessage              -> WindowShowMessage
    SWindowShowMessageRequest       -> WindowShowMessageRequest
    SWindowLogMessage               -> WindowLogMessage
    STelemetryEvent                 -> TelemetryEvent
    SClientRegisterCapability       -> ClientRegisterCapability
    SClientUnregisterCapability     -> ClientUnregisterCapability
    SWorkspaceApplyEdit             -> WorkspaceApplyEdit
    STextDocumentPublishDiagnostics -> TextDocumentPublishDiagnostics
  --}}}
  toSing = \case --{{{
    WindowShowMessage              -> SomeSing SWindowShowMessage
    WindowShowMessageRequest       -> SomeSing SWindowShowMessageRequest
    WindowLogMessage               -> SomeSing SWindowLogMessage
    TelemetryEvent                 -> SomeSing STelemetryEvent
    ClientRegisterCapability       -> SomeSing SClientRegisterCapability
    ClientUnregisterCapability     -> SomeSing SClientUnregisterCapability
    WorkspaceApplyEdit             -> SomeSing SWorkspaceApplyEdit
    TextDocumentPublishDiagnostics -> SomeSing STextDocumentPublishDiagnostics
  --}}}
  --}}}

-------------------------------------------------------------------------------
-- Foo
-------------------------------------------------------------------------------

data X = Client | Server

type family Method (x :: X) = p | p -> x where
  Method 'Client = ClientMethod
  Method 'Server = ServerMethod

type family Actor m :: X where
  Actor ClientMethodK = 'Client
  Actor ServerMethodK = 'Server

-- なんだかなあ
class (SingKind k, Method (Actor k) ~ Demote k
      ,Show (Method (Actor k)), JSONField (Method (Actor k))) => IsMethodKind k
instance IsMethodKind ClientMethodK
instance IsMethodKind ServerMethodK

-------------------------------------------------------------------------------
-- Constraint Util
-------------------------------------------------------------------------------

type Top = (() :: Constraint)
class GHC.Exts.Any => Bottom where no :: a

bottom :: Bottom :- a
bottom = Sub no

