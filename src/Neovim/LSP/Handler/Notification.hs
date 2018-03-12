
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.Handler.Notification
  --( notificationHandler
  --)
  where

import           Control.Lens
import           Control.Monad                       (forM_, forever)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Extensible                     (FieldOptic)
import           Data.Aeson                          as J hiding (Error)
import qualified Data.ByteString.Lazy.Char8          as B

import           Neovim
import qualified Neovim.Quickfix                     as Q
import           Neovim.Quickfix                     (QuickfixListItem)
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type

notificationHandler :: Handler
notificationHandler = Handler notificationPred notificationHandlerAction

notificationPred :: InMessage -> Bool
notificationPred SomeNot{} = True
notificationPred _ = False

notificationHandlerAction :: HandlerAction ()
notificationHandlerAction = forever @_ @() @() $ do
    msg <- pull
    debugM $ "notificationHandler got " ++ B.unpack (encode msg)
    case msg of
      SomeNot (noti :: ServerNotification m) -> case singByProxy noti of
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
               -> HandlerAction ()
showDiagnotics (Notification noti) = do
    let params      = noti^. #params
        uri         = params^. #uri
        diagnostics = params^. #diagnostics
    forM_ diagnostics $ \d -> liftIO $ do
      putStrLn ""
      print $ diagnosticToQFItems uri d
      -- putStrLn $ T.unpack $ d^. #message -- readale message for debug
      putStrLn ""
    return ()

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
      , Q.col           = Just (col, True)
      , Q.nr            = Nothing
      , Q.text          = text'
      , Q.errorType     = errorType
      }
      where
        start = d^. (#range :: FieldOptic "range") . #start
        lnum = 1 + round (start^. #line)
        col  = 1 + round (start^. #character)
        errorType = case d^. #severity of
            Some Error -> Q.Error
            _ -> Q.Warning
        text' = case d^. #source of
            None -> ""
            Some n -> T.pack $ "[" ++ n ++ "]"
    rest = flip map (T.lines (d^. #message)) $ \msg -> Q.QFItem
      { Q.bufOrFile     = Right ""
      , Q.lnumOrPattern = Right ""
      , Q.col           = Nothing
      , Q.nr            = Nothing
      , Q.text          = msg
      , Q.errorType     = Q.Warning
      }

