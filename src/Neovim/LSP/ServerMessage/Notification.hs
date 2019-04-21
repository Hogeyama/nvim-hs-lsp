
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.ServerMessage.Notification
  ( notificationHandler
  ) where

import           RIO
import qualified RIO.Map                  as M
import qualified RIO.Text                 as T

import           Control.Lens             ((%~))
import           Data.Generics.Product    (field)

import           LSP
import           Util
import           Neovim                   ()
import           Neovim.LSP.Base
import           Neovim.LSP.Util

notificationHandler :: Worker
notificationHandler = Worker "noti" notificationHandler'

notificationHandler' :: WorkerM ()
notificationHandler' = forever $ loggingErrorImmortal $ do
    receiveMessage >>= \case
      SomeNoti noti -> case singByProxy noti of
        STextDocumentPublishDiagnostics -> showDiagnotics noti
        SWindowLogMessage -> windowLogMessage noti
        STelemetryEvent -> telemetryEvent noti
        SWindowShowMessage -> windowShowMessage noti
        SServerCancel ->
          logError "notificationHandler: ServerCancel: not implemented"
        SServerNotificationMisc _ ->
          logError "notificationHandler: Misc: not implemented"
      _ -> return ()

-------------------------------------------------------------------------------
-- TextDocumentPublishDiagnostics
-------------------------------------------------------------------------------

showDiagnotics :: ServerNotification 'TextDocumentPublishDiagnosticsK
               -> WorkerM ()
showDiagnotics (Notification noti) = do
    let uri         = noti^. #params.__#uri
        diagnostics = noti^. #params.__#diagnostics
    modifyContext $ (field @"diagnosticsMap") %~ M.insert uri diagnostics
    -- 今開いてるBufferは優先的に表示する
    -- curiとuriは必ずしも一致しないことに注意
    --    e.g. hieではapp/Main.hsを編集するとsrc/Lib.hsのdiagも
    --         送られてくることがある
    whenM (readContext . view $
            field @"lspConfig".
            field @"autoLoadQuickfix") $ do
      allDiagnostics <- readContext . view $ field @"diagnosticsMap"
      curi <- getBufUri =<< nvim_get_current_buf
      replaceQfList $ diagnosticsToQfItems curi allDiagnostics

-------------------------------------------------------------------------------
-- WindowLogMessage
-------------------------------------------------------------------------------

windowLogMessage :: ServerNotification 'WindowLogMessageK
                 -> WorkerM ()
windowLogMessage (Notification noti) = do
    let type'   = noti^. #params.__#type
        message = noti^. #params.__#message
        log' l = do
          -- nvimEchom $ "window/logMessage: " <> T.unpack message
          l $ "window/logMessage: " <> displayShow message
    caseOfEnum type'
      $ (MatchEnum @"error"   $ log' logError)
     <! (MatchEnum @"warning" $ log' logWarn)
     <! (MatchEnum @"info"    $ log' logInfo)
     <! (MatchEnum @"log"     $ log' (logInfoS "log"))
     <! nil

-------------------------------------------------------------------------------
-- WindowShowMessage
-------------------------------------------------------------------------------

windowShowMessage :: ServerNotification 'WindowShowMessageK
                  -> WorkerM ()
windowShowMessage (Notification noti) = do
    let type'   = noti^. #params.__#type
        message = T.unpack $ noti^. #params.__#message
    caseOfEnum type'
       $ (MatchEnum @"error"   $ nvimEchoe $ "LSP: Error: "   <> message)
      <! (MatchEnum @"warning" $ nvimEchow $ "LSP: Warning: " <> message)
      <! (MatchEnum @"info"    $ nvimEchom $ "LSP: Info: "    <> message)
      <! (MatchEnum @"log"     $ nvimEchom $ "LSP: Log: "     <> message)
      <! nil

-------------------------------------------------------------------------------
-- TelemetryEvent
-------------------------------------------------------------------------------

telemetryEvent :: ServerNotification 'TelemetryEventK
               -> WorkerM ()
telemetryEvent (Notification noti) = do
    let params = noti^. #params
    logInfo $ "telemetry/event: " <> displayShow params

