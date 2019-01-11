
module Neovim.LSP.Protocol.Type.Proof
  ( prfClientReq
  , prfClientNoti
  , prfClientResp
  , prfServerReq
  , prfServerNoti
  , prfServerResp
  ) where

import           RIO

import           Data.Constraint
import           Data.Singletons.TypeLits
import           Neovim.LSP.Protocol.Type.Interfaces
import           Neovim.LSP.Protocol.Type.Method

prfClientReq :: forall (m :: ClientRequestMethodK). Sing m -> Dict (ImplRequest m)
prfClientReq = \case -- {{{
    SInitialize                      -> Dict
    SShutdown                        -> Dict
    SWorkspaceSymbol                 -> Dict
    SWorkspaceExecuteCommand         -> Dict
    STextDocumentWillSaveWaitUntil   -> Dict
    STextDocumentCompletion          -> Dict
    SCompletionItemResolve           -> Dict
    STextDocumentHover               -> Dict
    STextDocumentSignatureHelp       -> Dict
    STextDocumentReferences          -> Dict
    STextDocumentDocumentHighlight   -> Dict
    STextDocumentDocumentSymbol      -> Dict
    STextDocumentFormatting          -> Dict
    STextDocumentRangeFormatting     -> Dict
    STextDocumentOnTypeFormatting    -> Dict
    STextDocumentDefinition          -> Dict
    STextDocumentCodeAction          -> Dict
    STextDocumentCodeLens            -> Dict
    SCodeLensResolve                 -> Dict
    STextDocumentDocumentLink        -> Dict
    SDocumentLinkResolve             -> Dict
    STextDocumentRename              -> Dict
    SClientRequestMisc SSym          -> Dict
-- }}}

prfClientNoti :: forall (m :: ClientNotificationMethodK). Sing m -> Dict (ImplNotification m)
prfClientNoti = \case-- {{{
    SInitialized                     -> Dict
    SExit                            -> Dict
    SClientCancel                    -> Dict
    SWorkspaceDidChangeConfiguration -> Dict
    SWorkspaceDidChangeWatchedFiles  -> Dict
    STextDocumentDidOpen             -> Dict
    STextDocumentDidChange           -> Dict
    STextDocumentWillSave            -> Dict
    STextDocumentDidSave             -> Dict
    STextDocumentDidClose            -> Dict
    SClientNotificationMisc SSym     -> Dict
  -- }}}

prfServerResp :: forall (m :: ClientRequestMethodK). Sing m -> Dict (ImplResponse m)
prfServerResp = \case  -- {{{
    SInitialize                      -> Dict
    SShutdown                        -> Dict
    SWorkspaceSymbol                 -> Dict
    SWorkspaceExecuteCommand         -> Dict
    STextDocumentWillSaveWaitUntil   -> Dict
    STextDocumentCompletion          -> Dict
    SCompletionItemResolve           -> Dict
    STextDocumentHover               -> Dict
    STextDocumentSignatureHelp       -> Dict
    STextDocumentReferences          -> Dict
    STextDocumentDocumentHighlight   -> Dict
    STextDocumentDocumentSymbol      -> Dict
    STextDocumentFormatting          -> Dict
    STextDocumentRangeFormatting     -> Dict
    STextDocumentOnTypeFormatting    -> Dict
    STextDocumentDefinition          -> Dict
    STextDocumentCodeAction          -> Dict
    STextDocumentCodeLens            -> Dict
    SCodeLensResolve                 -> Dict
    STextDocumentDocumentLink        -> Dict
    SDocumentLinkResolve             -> Dict
    STextDocumentRename              -> Dict
    SClientRequestMisc SSym          -> Dict
-- }}}

prfServerReq :: forall (m :: ServerRequestMethodK). Sing m -> Dict (ImplRequest m)
prfServerReq = \case -- {{{
  SWindowShowMessageRequest       -> Dict
  SClientRegisterCapability       -> Dict
  SClientUnregisterCapability     -> Dict
  SWorkspaceApplyEdit             -> Dict
  SServerRequestMisc SSym         -> Dict
-- }}}

prfServerNoti :: forall (m :: ServerNotificationMethodK). Sing m -> Dict (ImplNotification m)
prfServerNoti = \case -- {{{
  SWindowShowMessage              -> Dict
  SWindowLogMessage               -> Dict
  STelemetryEvent                 -> Dict
  STextDocumentPublishDiagnostics -> Dict
  SServerCancel                   -> notImplemented
  SServerNotificationMisc SSym    -> Dict
-- }}}

prfClientResp :: forall (m :: ServerRequestMethodK). Sing m -> Dict (ImplResponse m)
prfClientResp = \case -- {{{
  SWindowShowMessageRequest       -> notImplemented
  SClientRegisterCapability       -> notImplemented
  SClientUnregisterCapability     -> notImplemented
  SWorkspaceApplyEdit             -> Dict
  SServerRequestMisc SSym         -> Dict
-- }}}

notImplemented :: HasCallStack => a
notImplemented = error "Neovim.LSP.Protocol.Type.Proof: not implemented"

