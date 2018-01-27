
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# OPTIONS_GHC -Wall       #-}

module Neovim.LSP.Protocol.Type.Singleton where

import           Data.Constraint
import           Debug.Trace                         (trace)
import           Neovim.LSP.Protocol.Type.Interfaces
import           Neovim.LSP.Protocol.Type.Method

isServerRequest :: Sing (m :: ServerMethodK) -> Maybe (Dict (ImplRequest m))
isServerRequest = \case
  -- 実装してないのでかけない
  --SWindowShowMessage              -> Just Dict
  --SWindowShowMessageRequest       -> Just Dict
  --SWindowLogMessage               -> Just Dict
  --STelemetryEvent                 -> Just Dict
  --SClientRegisterCapability       -> Just Dict
  --SClientUnregisterCapability     -> Just Dict
  --SWorkspaceApplyEdit             -> Just Dict
  --STextDocumentPublishDiagnostics -> Just Dict
  _ -> trace "warning: isServerRequest: not implemented" Nothing

isServerNotification :: Sing (m :: ServerMethodK) -> Maybe (Dict (ImplNotification m))
isServerNotification = \case
  STextDocumentPublishDiagnostics -> Just Dict
  SWindowShowMessage              -> Just Dict
  SWindowLogMessage               -> Just Dict
  STelemetryEvent                 -> Just Dict
  _ -> trace "warning: isServerNotification: not implemented"  Nothing

isServerResponse :: Sing (m :: ClientMethodK) -> Maybe (Dict (ImplResponse m))
isServerResponse = \case
  SInitialize        -> Just Dict
  STextDocumentHover -> Just Dict
  SCancelRequest     -> Nothing -- Requestという名前だが実際はNotification
  _ -> trace "warning: isServerRequest: not implemented" Nothing

