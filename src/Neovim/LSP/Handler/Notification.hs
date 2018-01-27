
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.Handler.Notification
  --( notificationHandler
  --)
  where

import           Control.Lens
import           Control.Monad                       (forM_, forever)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Aeson                          as J hiding (Error)
import qualified Data.ByteString.Lazy.Char8          as B

import           Neovim
import qualified Neovim.Quickfix                     as Q
import           Neovim.Quickfix                     (QuickfixListItem)
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type
import qualified Neovim.LSP.Protocol.Type.Key        as K

notificationHandler :: Handler
notificationHandler = Handler notificationPred notificationHandlerAction

notificationPred :: InMessage -> Bool
notificationPred x = case methodOf x of
  Right TextDocumentPublishDiagnostics -> True
  Right WindowLogMessage               -> True
  Right WindowShowMessage              -> True
  Right TelemetryEvent                 -> True
  _ -> False

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
        SWindowShowMessageRequest ->
          let _ = show @(NotificationParam m) undefined
              _ = show @(NotificationParam 'WindowShowMessageRequestK) undefined
          in undefined
          -- ここでimpossibleになってほしい
          --    noti :: ServerNotification m
          --    SWindowShowMessageRequest :: Sing m
          --    IsNotification 'Server m
          -- が使える
          --    IsNotification 'Server WindowShowMessageRequest
          -- からabsurdが使えればいいんだが，
          -- 今のIsNotificationの定義も便利なので捨てたくない
          --    Show (NotificationParam 'Server WindowShowMessage)
          -- から矛盾が言えないかな
          --    + NotificationParamがdefinedならそのまま
          --    + そうでなければ Bottom (class Bottom where void :: Void)
          -- になるような制約が欲しい．結局これがわからなくて詰んだのであった
          -- ConstraintでOrが書ける良い？
          -- やっぱり定義を捨てるのがよいかな
        _ -> error "impossible" -- TODO これかっこわるいよなあ
      _ -> error "impossible"

-------------------------------------------------------------------------------
-- TextDocumentPublishDiagnostics
-------------------------------------------------------------------------------

showDiagnotics :: ServerNotification 'TextDocumentPublishDiagnosticsK
               -> HandlerAction ()
showDiagnotics (Notification noti) = do
    let uri         = noti ^. K.params.K.uri
        diagnostics = noti ^. K.params.K.diagnostics
    forM_ diagnostics $ \d -> liftIO $ do
      putStrLn ""
      print $ diagnosticToQFItems uri d
      putStrLn ""
    return ()

-------------------------------------------------------------------------------

--diagnosticToQFItem :: Uri -> Diagnostic -> QuickfixListItem Text
--diagnosticToQFItem uri d = Q.QFItem
--    { Q.bufOrFile     = Right $ T.pack $ uriToFilePath uri
--    , Q.lnumOrPattern = Left lnum
--    , Q.col           = Just (col, True)
--    , Q.nr            = Nothing
--    , Q.text          = d^.K.message
--    , Q.errorType     = errorType
--    }
--  where
--    lnum = 1 + round (d^.K.range.K.start.K.line)
--    col  = 1 + round (d^.K.range.K.start.K.character)
--    errorType = case d^.K.severity of
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
        lnum = 1 + round (d^.K.range.K.start.K.line)
        col  = 1 + round (d^.K.range.K.start.K.character)
        errorType = case d^.K.severity of
            Some Error -> Q.Error
            _ -> Q.Warning
        text' = case d^.K.source of
            None -> ""
            Some n -> T.pack $ "[" ++ n ++ "]"
    rest = flip map (T.lines (d^.K.message)) $ \msg -> Q.QFItem
      { Q.bufOrFile     = Right ""
      , Q.lnumOrPattern = Right ""
      , Q.col           = Nothing
      , Q.nr            = Nothing
      , Q.text          = msg
      , Q.errorType     = Q.Warning
      }

