{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TypeOperators   #-}

module Neovim.LSP.Protocol.Type.Proof
  ( entailClientNoti
  , entailClientReq
  , entailServerNoti
  , entailServerReq
  ) where

import           Data.Constraint                     hiding (Bottom, bottom)
import           Data.Singletons.TypeLits
import           Neovim.LSP.Protocol.Type.Interfaces
import           Neovim.LSP.Protocol.Type.Method

entailClientReq :: forall (m :: ClientMethodK). Sing m
                -> IsClientRequest m :- ImplRequest m
entailClientReq = \case-- {{{
    SInitialize                      -> Sub Dict
    SShutdown                        -> notImplemented
    SWorkspaceSymbol                 -> notImplemented
    SWorkspaceExecuteCommand         -> notImplemented
    STextDocumentWillSaveWaitUntil   -> notImplemented
    STextDocumentCompletion          -> notImplemented
    SCompletionItemResolve           -> notImplemented
    STextDocumentHover               -> Sub Dict
    STextDocumentSignatureHelp       -> Sub Dict
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
    SMisc SSym                       -> Sub Dict
    SInitialized                     -> bottom
    SCancelRequest                   -> bottom
    SExit                            -> bottom
    SWorkspaceDidChangeConfiguration -> bottom
    SWorkspaceDidChangeWatchedFiles  -> bottom
    STextDocumentDidOpen             -> bottom
    STextDocumentDidChange           -> bottom
    STextDocumentWillSave            -> bottom
    STextDocumentDidSave             -> bottom
    STextDocumentDidClose            -> bottom
  -- }}}

entailClientNoti :: forall (m :: ClientMethodK). Sing m
                 -> IsClientNotification m :- ImplNotification m
entailClientNoti = \case-- {{{
    SInitialized                     -> notImplemented
    SExit                            -> notImplemented -- TODO
    SWorkspaceDidChangeConfiguration -> notImplemented
    SWorkspaceDidChangeWatchedFiles  -> notImplemented
    STextDocumentDidOpen             -> Sub Dict
    STextDocumentDidChange           -> Sub Dict
    STextDocumentWillSave            -> notImplemented
    STextDocumentDidSave             -> Sub Dict
    STextDocumentDidClose            -> notImplemented
    SCancelRequest                   -> Sub Dict
    SInitialize                      -> bottom
    SShutdown                        -> bottom
    SWorkspaceSymbol                 -> bottom
    SWorkspaceExecuteCommand         -> bottom
    STextDocumentWillSaveWaitUntil   -> bottom
    STextDocumentCompletion          -> bottom
    SCompletionItemResolve           -> bottom
    STextDocumentHover               -> bottom
    STextDocumentSignatureHelp       -> bottom
    STextDocumentReferences          -> bottom
    STextDocumentDocumentHighlight   -> bottom
    STextDocumentDocumentSymbol      -> bottom
    STextDocumentFormatting          -> bottom
    STextDocumentRangeFormatting     -> bottom
    STextDocumentOnTypeFormatting    -> bottom
    STextDocumentDefinition          -> bottom
    STextDocumentCodeAction          -> bottom
    STextDocumentCodeLens            -> bottom
    SCodeLensResolve                 -> bottom
    STextDocumentDocumentLink        -> bottom
    SDocumentLinkResolve             -> bottom
    STextDocumentRename              -> bottom
    SMisc SSym                       -> bottom
  -- }}}

entailServerReq :: forall (m :: ServerMethodK). Sing m
                -> IsServerRequest m :- ImplRequest m
entailServerReq = notImplemented

entailServerNoti :: forall (m :: ServerMethodK). Sing m
                 -> IsServerNotification m :- ImplNotification m
entailServerNoti = notImplemented

notImplemented :: a
notImplemented = error "Neovim.LSP.Protocol.Type.Proof: not implemented"

