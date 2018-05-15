
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.LspPlugin.Notification
  ( notificationHandler
  )
  where

import           Control.Lens
import           Control.Monad            (forever)
import qualified Data.Map                 as M
import qualified System.Log.Logger        as L

import           Neovim                   hiding (Plugin)
import           Neovim.LSP.Base
import           Neovim.LSP.Util
import           Neovim.LSP.Protocol.Type

notificationHandler :: Plugin
notificationHandler = Plugin "noti" notificationPluginAction

notificationPluginAction :: PluginAction ()
notificationPluginAction = do
  setLogLevel L.WARNING
  forever $ do
    msg <- pull
    case msg of
      SomeNoti (noti :: ServerNotification m) -> case singByProxy noti of
        STextDocumentPublishDiagnostics -> do
          showDiagnotics noti
        SWindowShowMessage -> do
          errorM "notificationHandler: WindowShowMessage: not implemented"
        SWindowLogMessage -> do
          errorM "notificationHandler: WindowLogMessage: not implemented"
        STelemetryEvent -> do
          errorM "notificationHandler: TelemetryEvent: not implemented"
        SServerCancel -> do
          errorM "notificationHandler: ServerCancel: not implemented"
        SServerNotificationMisc _ -> do
          errorM "notificationHandler: Misc: not implemented"
      _ -> return ()

-------------------------------------------------------------------------------
-- TextDocumentPublishDiagnostics
-------------------------------------------------------------------------------

showDiagnotics :: ServerNotification 'TextDocumentPublishDiagnosticsK
               -> PluginAction ()
showDiagnotics (Notification noti) = do
    let params      = noti^. #params
        uri         = params^. #uri
        diagnostics = params^. #diagnostics
    modifyContext $
      over (lspOtherState.diagnosticsMap) $
      M.insert uri diagnostics

    -- 今開いてるBufferは優先的に表示する
    -- curiとuriは必ずしも一致しないことに注意
    --    e.g. hieではapp/Main.hsを編集するとsrc/Lib.hsのdiagも送られてくることがある
    whenM (readContext (view (lspConfig.autoLoadQuickfix))) $ do
      allDiagnostics <- readContext $ view (lspOtherState.diagnosticsMap)
      curi <- getBufUri =<< nvim_get_current_buf'
      let qfItems = diagnosticsToQfItems curi allDiagnostics
      replaceQfList qfItems

