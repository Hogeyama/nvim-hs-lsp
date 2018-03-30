
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
import           Control.Monad            (forever, when)
import           Data.Extensible
import           Data.Function            (on)
import           Data.List                (sortBy)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified System.Log.Logger        as L

import           Neovim                   hiding (Plugin)
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util
import           Neovim.Quickfix          (QuickfixListItem)
import qualified Neovim.Quickfix          as Q

notificationHandler :: Plugin
notificationHandler = Plugin "noti" notificationPluginAction

notificationPluginAction :: PluginAction ()
notificationPluginAction = do
  --setLogLevel L.WARNING
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
        qfItems     = concatMap (diagnosticToQfItems uri)
                        $ sortBy (compare `on` (^. #severity)) diagnostics
    debugM $ "showDiagnotics: " ++ show noti
    debugM $ "showDiagnotics: " ++ show qfItems
    replaceQfList qfItems
    when (null qfItems) $ nvimEcho "textDocument/publishDiagnostics: no error"

-------------------------------------------------------------------------------

type QfItem = Record
  '[ "filename" >: Option String
   , "lnum"     >: Option Int
   , "col"      >: Option Int
   , "type"     >: Option String
   , "text"     >: String
   , "valid"    >: Option Bool
   ]

replaceQfList :: [QfItem] -> Neovim env ()
replaceQfList qs = void $ vim_call_function "setqflist" $ qs +: ['r'] +: []

diagnosticToQfItems :: Uri -> Diagnostic -> [QfItem]
diagnosticToQfItems uri d = header : rest
  where
    header = #filename @= Some (uriToFilePath uri)
          <: #lnum     @= Some lnum
          <: #col      @= Some col
          <: #type     @= Some errorType
          <: #text     @= text
          <: #valid    @= Some True
          <: nil
      where
        start = d^. (#range :: FieldOptic "range") . #start
        lnum = 1 + start^. #line
        col  = 1 + start^. #character
        errorType = case d^. #severity of
            Some Error        -> "E"
            Some Warning      -> "W"
            Some Information  -> "I"
            Some Hint         -> "H"
            _                 -> "W"
        text = case d^. #source of
            None   -> ""
            Some n -> "[" ++ n ++ "]"
    rest = flip map (T.lines (d^. #message)) $ \msg ->
             #filename @= None
          <: #lnum     @= None
          <: #col      @= None
          <: #type     @= None
          <: #text     @= T.unpack msg
          <: #valid    @= Some False
          <: nil

--diagnosticToQFItems :: Uri -> Diagnostic -> [QuickfixListItem Text]
--diagnosticToQFItems uri d = header : rest
--  where
--    header =  Q.QFItem
--      { Q.bufOrFile     = Right $ T.pack $ uriToFilePath uri
--      , Q.lnumOrPattern = Left lnum
--      , Q.col           = Q.ByteIndexColumn col
--      , Q.nr            = Nothing
--      , Q.text          = text'
--      , Q.errorType     = errorType
--      }
--      where
--        start = d^. (#range :: FieldOptic "range") . #start
--        lnum = 1 + start^. #line
--        col  = 1 + start^. #character
--        errorType = case d^. #severity of
--            Some Error -> Q.Error
--            _          -> Q.Warning
--        text' = case d^. #source of
--            None   -> ""
--            Some n -> T.pack $ "[" ++ n ++ "]"
--    rest = flip map (T.lines (d^. #message)) $ \msg -> Q.QFItem
--      { Q.bufOrFile     = Right ""
--      , Q.lnumOrPattern = Right ""
--      , Q.col           = Q.NoColumn
--      , Q.nr            = Nothing
--      , Q.text          = msg
--      , Q.errorType     = Q.Warning
--      }

