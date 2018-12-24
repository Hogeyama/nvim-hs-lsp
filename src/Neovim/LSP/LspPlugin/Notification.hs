
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.LspPlugin.Notification
  ( notificationHandler
  ) where

import           RIO
import qualified RIO.Map                  as M
import qualified RIO.Text                 as T

import           Control.Lens             ((%~))
import           Data.Extensible.Rexport

import           Neovim                   hiding (Plugin, whenM)
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

notificationHandler :: Plugin
notificationHandler = Plugin "noti" notificationPluginAction

notificationPluginAction :: PluginAction ()
notificationPluginAction = forever $ loggingErrorImmortal $ do
    msg <- pull
    case msg of
      SomeNoti noti -> case singByProxy noti of
        STextDocumentPublishDiagnostics -> showDiagnotics noti
        SWindowLogMessage -> windowLogMessage noti
        STelemetryEvent -> telemetryEvent noti
        SWindowShowMessage -> windowShowMessage noti
        SServerCancel -> logError "notificationHandler: ServerCancel: not implemented"
        SServerNotificationMisc _ -> logError "notificationHandler: Misc: not implemented"
      _ -> return ()

-------------------------------------------------------------------------------
-- TextDocumentPublishDiagnostics
-------------------------------------------------------------------------------

showDiagnotics :: ServerNotification 'TextDocumentPublishDiagnosticsK
               -> PluginAction ()
showDiagnotics (Notification noti) = do
    let uri         = noti^. #params.__#uri
        diagnostics = noti^. #params.__#diagnostics
    modifyContext $ #otherState.diagnosticsMap %~ M.insert uri diagnostics
    -- 今開いてるBufferは優先的に表示する
    -- curiとuriは必ずしも一致しないことに注意
    --    e.g. hieではapp/Main.hsを編集するとsrc/Lib.hsのdiagも送られてくることがある
    whenM (readContext . view $ #lspConfig.autoLoadQuickfix) $ do
      allDiagnostics <- readContext . view $ #otherState.diagnosticsMap
      curi <- getBufUri =<< nvim_get_current_buf'
      replaceQfList $ diagnosticsToQfItems curi allDiagnostics

-------------------------------------------------------------------------------
-- WindowLogMessage
-------------------------------------------------------------------------------

windowLogMessage :: ServerNotification 'WindowLogMessageK
                 -> PluginAction ()
windowLogMessage (Notification noti) = do
    let type'   = noti^. #params.__#type
        message = noti^. #params.__#message
        log' l = do
          -- nvimEchom $ "window/logMessage: " <> T.unpack message
          l $ "window/logMessage: " <> displayShow message
    caseOfEnumAs type'
      $ (MatchEnum @"error"   $ log' logError)
     <! (MatchEnum @"warning" $ log' logWarn)
     <! (MatchEnum @"info"    $ log' logInfo)
     <! (MatchEnum @"log"     $ log' (logInfoS "log"))
     <! nil

-------------------------------------------------------------------------------
-- WindowShowMessage
-------------------------------------------------------------------------------

windowShowMessage :: ServerNotification 'WindowShowMessageK
                  -> PluginAction ()
windowShowMessage (Notification noti) = do
    let type'   = noti^. #params.__#type
        message = noti^. #params.__#message
    caseOfEnumAs type'
       $ (MatchEnum @"error"   $ nvimEchoe $ "LSP: Error: " <> T.unpack message)
      <! (MatchEnum @"warning" $ nvimEchow $ "LSP: Warning: " <> T.unpack message)
      <! (MatchEnum @"info"    $ nvimEchom $ "LSP: Info: " <> T.unpack message)
      <! (MatchEnum @"log"     $ nvimEchom $ "LSP: Log: " <> T.unpack message)
      <! nil

-------------------------------------------------------------------------------
-- TelemetryEvent
-------------------------------------------------------------------------------

telemetryEvent :: ServerNotification 'TelemetryEventK
               -> PluginAction ()
telemetryEvent (Notification noti) = do
    let params = noti^. #params
    logInfo $ "telemetry/event: " <> displayShow params

