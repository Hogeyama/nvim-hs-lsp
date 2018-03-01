
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# OPTIONS_GHC -Wall       #-}

-- 多分使わないで済むmodule

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

-- あとでけす
isServerNotification :: Sing (m :: ServerMethodK) -> Maybe (Dict (ImplNotification m))
isServerNotification = \case
  --SSNoti STextDocumentPublishDiagnostics -> Just Dict
  --SSNoti SWindowShowMessage              -> Just Dict
  --SSNoti SWindowLogMessage               -> Just Dict
  --SSNoti STelemetryEvent                 -> Just Dict
  _ -> trace "warning: isServerNotification: not implemented"  Nothing

