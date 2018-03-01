{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TypeOperators   #-}

module Neovim.LSP.Protocol.Type.Proof
  ( prfClientReq
  , prfClientNoti
  , prfClientRes
  , prfServerReq
  , prfServerNoti
  , prfServerRes
  ) where

import           Data.Constraint
import           Data.Singletons.TypeLits
import           Neovim.LSP.Protocol.Type.Interfaces
import           Neovim.LSP.Protocol.Type.Method

prfClientReq :: forall (m :: ClientRequestMethodK). Sing m -> Dict (ImplRequest m)
prfClientReq = \case -- {{{
    SInitialize                      -> Dict
    SShutdown                        -> notImplemented
    SWorkspaceSymbol                 -> notImplemented
    SWorkspaceExecuteCommand         -> notImplemented
    STextDocumentWillSaveWaitUntil   -> notImplemented
    STextDocumentCompletion          -> notImplemented
    SCompletionItemResolve           -> notImplemented
    STextDocumentHover               -> Dict
    STextDocumentSignatureHelp       -> Dict
    STextDocumentReferences          -> notImplemented
    STextDocumentDocumentHighlight   -> notImplemented
    STextDocumentDocumentSymbol      -> notImplemented
    STextDocumentFormatting          -> notImplemented
    STextDocumentRangeFormatting     -> notImplemented
    STextDocumentOnTypeFormatting    -> notImplemented
    STextDocumentDefinition          -> notImplemented
    STextDocumentCodeAction          -> notImplemented
    STextDocumentCodeLens            -> notImplemented
    SCodeLensResolve                 -> notImplemented
    STextDocumentDocumentLink        -> notImplemented
    SDocumentLinkResolve             -> notImplemented
    STextDocumentRename              -> notImplemented
    SClientRequestMisc SSym          -> Dict
-- }}}

prfClientNoti :: forall (m :: ClientNotificationMethodK). Sing m -> Dict (ImplNotification m)
prfClientNoti = \case-- {{{
    SInitialized                     -> notImplemented
    SExit                            -> notImplemented -- TODO
    SWorkspaceDidChangeConfiguration -> notImplemented
    SWorkspaceDidChangeWatchedFiles  -> notImplemented
    STextDocumentDidOpen             -> Dict
    STextDocumentDidChange           -> Dict
    STextDocumentWillSave            -> notImplemented
    STextDocumentDidSave             -> Dict
    STextDocumentDidClose            -> notImplemented
    SClientCancel                    -> Dict
    SClientNotificationMisc SSym     -> Dict
  -- }}}

prfServerRes :: forall (m :: ClientRequestMethodK). Sing m -> Dict (ImplResponse m)
prfServerRes = \case  -- {{{
    SInitialize                      -> Dict
    SShutdown                        -> notImplemented
    SWorkspaceSymbol                 -> notImplemented
    SWorkspaceExecuteCommand         -> notImplemented
    STextDocumentWillSaveWaitUntil   -> notImplemented
    STextDocumentCompletion          -> notImplemented
    SCompletionItemResolve           -> notImplemented
    STextDocumentHover               -> Dict
    STextDocumentSignatureHelp       -> Dict
    STextDocumentReferences          -> notImplemented
    STextDocumentDocumentHighlight   -> notImplemented
    STextDocumentDocumentSymbol      -> notImplemented
    STextDocumentFormatting          -> notImplemented
    STextDocumentRangeFormatting     -> notImplemented
    STextDocumentOnTypeFormatting    -> notImplemented
    STextDocumentDefinition          -> notImplemented
    STextDocumentCodeAction          -> notImplemented
    STextDocumentCodeLens            -> notImplemented
    SCodeLensResolve                 -> notImplemented
    STextDocumentDocumentLink        -> notImplemented
    SDocumentLinkResolve             -> notImplemented
    STextDocumentRename              -> notImplemented
    SClientRequestMisc SSym          -> Dict
-- }}}

prfServerReq :: forall (m :: ServerRequestMethodK). Sing m -> Dict (ImplRequest m)
prfServerReq = \case -- {{{
  SWindowShowMessageRequest       -> notImplemented
  SClientRegisterCapability       -> notImplemented
  SClientUnregisterCapability     -> notImplemented
  SWorkspaceApplyEdit             -> notImplemented
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

prfClientRes :: forall (m :: ServerRequestMethodK). Sing m -> Dict (ImplResponse m)
prfClientRes = \case -- {{{
  SWindowShowMessageRequest       -> notImplemented
  SClientRegisterCapability       -> notImplemented
  SClientUnregisterCapability     -> notImplemented
  SWorkspaceApplyEdit             -> notImplemented
  SServerRequestMisc SSym         -> Dict
-- }}}

notImplemented :: a
notImplemented = error "Neovim.LSP.Protocol.Type.Proof: not implemented"

