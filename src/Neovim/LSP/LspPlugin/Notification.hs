
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.LspPlugin.Notification
  ( notificationHandler
  ) where

import           RIO
import qualified RIO.Map                  as M
import qualified RIO.Text                 as T

import           Control.Lens             ((%~))
import           Data.Generics.Product    (field)

import           LSP
import           Util
import           Neovim                   hiding (Plugin, whenM)
import           Neovim.LSP.Base
import           Neovim.LSP.Util

notificationHandler :: Worker
notificationHandler = Worker "noti" notificationWorkerAction

notificationWorkerAction :: WorkerAction ()
notificationWorkerAction = forever $ loggingErrorImmortal $ do
    receiveMessage >>= \case
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
               -> WorkerAction ()
showDiagnotics (Notification noti) = do
    let uri         = noti^. #params.__#uri
        diagnostics = noti^. #params.__#diagnostics
    modifyContext $ (field @"diagnosticsMap") %~ M.insert uri diagnostics
    -- 今開いてるBufferは優先的に表示する
    -- curiとuriは必ずしも一致しないことに注意
    --    e.g. hieではapp/Main.hsを編集するとsrc/Lib.hsのdiagも送られてくることがある
    whenM (readContext . view $
            field @"lspConfig".
            field @"autoLoadQuickfix") $ do
      allDiagnostics <- readContext . view $ field @"diagnosticsMap"
      curi <- getBufUri =<< nvim_get_current_buf'
      replaceQfList $ diagnosticsToQfItems curi allDiagnostics

-------------------------------------------------------------------------------
-- WindowLogMessage
-------------------------------------------------------------------------------

windowLogMessage :: ServerNotification 'WindowLogMessageK
                 -> WorkerAction ()
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
                  -> WorkerAction ()
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
               -> WorkerAction ()
telemetryEvent (Notification noti) = do
    let params = noti^. #params
    logInfo $ "telemetry/event: " <> displayShow params

