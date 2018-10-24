
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.LspPlugin.Notification
  ( notificationHandler
  ) where

import           RIO
import qualified RIO.Map                  as M

import           Control.Lens             ((%~))

import           Neovim                   hiding (Plugin, whenM)
import           Neovim.LSP.Base
import           Neovim.LSP.Util
import           Neovim.LSP.Protocol.Type

notificationHandler :: Plugin
notificationHandler = Plugin "noti" notificationPluginAction

notificationPluginAction :: PluginAction ()
notificationPluginAction = forever $ loggingErrorImmortal $ do
    msg <- pull
    case msg of
      SomeNoti (noti :: ServerNotification m) -> case singByProxy noti of
        STextDocumentPublishDiagnostics -> do
          showDiagnotics noti
        SWindowShowMessage -> do
          logError "notificationHandler: WindowShowMessage: not implemented"
        SWindowLogMessage -> do
          logError "notificationHandler: WindowLogMessage: not implemented"
        STelemetryEvent -> do
          logError "notificationHandler: TelemetryEvent: not implemented"
        SServerCancel -> do
          logError "notificationHandler: ServerCancel: not implemented"
        SServerNotificationMisc _ -> do
          logError "notificationHandler: Misc: not implemented"
      _ -> return ()

-------------------------------------------------------------------------------
-- TextDocumentPublishDiagnostics
-------------------------------------------------------------------------------

showDiagnotics :: ServerNotification 'TextDocumentPublishDiagnosticsK
               -> PluginAction ()
showDiagnotics (Notification noti) = do
    let uri         = noti^.__#params.__#uri
        diagnostics = noti^.__#params.__#diagnostics
    logError $ displayShow diagnostics
    modifyContext $ #otherState.diagnosticsMap %~ M.insert uri diagnostics
    -- 今開いてるBufferは優先的に表示する
    -- curiとuriは必ずしも一致しないことに注意
    --    e.g. hieではapp/Main.hsを編集するとsrc/Lib.hsのdiagも送られてくることがある
    whenM (readContext . view $ #lspConfig.autoLoadQuickfix) $ do
      allDiagnostics <- readContext . view $ #otherState.diagnosticsMap
      curi <- getBufUri =<< nvim_get_current_buf'
      replaceQfList $ diagnosticsToQfItems curi allDiagnostics

