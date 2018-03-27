
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.LspPlugin.Notification
  --( notificationHandler
  --)
  where

import           Control.Lens
import           Control.Monad                       (forever, when)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.List                           (sortBy)
import           Data.Function                       (on)
import           Data.Extensible                     (FieldOptic)
--import           Data.Aeson                          as J hiding (Error)
--import qualified Data.ByteString.Lazy.Char8          as B

--import           Neovim                              hiding (Plugin)
import qualified Neovim.Quickfix                     as Q
import           Neovim.Quickfix                     (QuickfixListItem)
import           Neovim.LSP.Base
import           Neovim.LSP.Util
import           Neovim.LSP.Protocol.Type

notificationHandler :: Plugin
notificationHandler = Plugin notificationPred notificationPluginAction

notificationPred :: InMessage -> Bool
notificationPred SomeNoti{} = True
notificationPred _ = False

notificationPluginAction :: PluginAction ()
notificationPluginAction = forever @_ @() @() $ do
    msg <- pull
    --debugM $ "notificationHandler got " ++ B.unpack (encode msg)
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
      _ -> error "impossible"

-------------------------------------------------------------------------------
-- TextDocumentPublishDiagnostics
-------------------------------------------------------------------------------

showDiagnotics :: ServerNotification 'TextDocumentPublishDiagnosticsK
               -> PluginAction ()
showDiagnotics (Notification noti) = do
    let params      = noti^. #params
        uri         = params^. #uri
        diagnostics = params^. #diagnostics
        qfItems     = concatMap (diagnosticToQFItems uri)
                        $ sortBy (compare `on` (^. #severity)) diagnostics
    Q.setqflist qfItems Q.Replace
    when (null qfItems) $ nvimEcho "textDocument/publishDiagnostics: no error"

-------------------------------------------------------------------------------

--diagnosticToQFItem :: Uri -> Diagnostic -> QuickfixListItem Text
--diagnosticToQFItem uri d = Q.QFItem
--    { Q.bufOrFile     = Right $ T.pack $ uriToFilePath uri
--    , Q.lnumOrPattern = Left lnum
--    , Q.col           = Just (col, True)
--    , Q.nr            = Nothing
--    , Q.text          = d^. #message
--    , Q.errorType     = errorType
--    }
--  where
--    start = d^. (#range :: FieldOptic "range") . #start
--    lnum = 1 + round (start^. #line)
--    col  = 1 + round (start^. #character)
--    errorType = case d^. #severity of
--      Some Error -> Q.Error
--      _ -> Q.Warning

diagnosticToQFItems :: Uri -> Diagnostic -> [QuickfixListItem Text]
diagnosticToQFItems uri d = header : rest
  where
    header =  Q.QFItem
      { Q.bufOrFile     = Right $ T.pack $ uriToFilePath uri
      , Q.lnumOrPattern = Left lnum
      , Q.col           = Q.ByteIndexColumn col
      , Q.nr            = Nothing
      , Q.text          = text'
      , Q.errorType     = errorType
      }
      where
        start = d^. (#range :: FieldOptic "range") . #start
        lnum = 1 + start^. #line
        col  = 1 + start^. #character
        errorType = case d^. #severity of
            Some Error -> Q.Error
            _ -> Q.Warning
        text' = case d^. #source of
            None -> ""
            Some n -> T.pack $ "[" ++ n ++ "]"
    rest = flip map (T.lines (d^. #message)) $ \msg -> Q.QFItem
      { Q.bufOrFile     = Right ""
      , Q.lnumOrPattern = Right ""
      , Q.col           = Q.NoColumn
      , Q.nr            = Nothing
      , Q.text          = msg
      , Q.errorType     = Q.Warning
      }

