{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.LspPlugin.Response
  ( responseHandler
  )
  where

import           Control.Lens
import           Control.Monad              (forever)
import           Data.Aeson                 as J
import qualified Data.ByteString.Lazy.Char8 as B

import qualified Data.Text                  as T
import           Neovim                     hiding (Plugin)
import           Neovim.LSP.Base
import           Neovim.LSP.Util
import           Neovim.LSP.Protocol.Type

responseHandler :: Plugin
responseHandler = Plugin responsePred responsePluginAction

responsePred :: InMessage -> Bool
responsePred SomeResp{} = True
responsePred _          = False

responsePluginAction :: PluginAction ()
responsePluginAction = forever @_ @() @() $ do
  hoge <- pull
  debugM $ "responseHandler got " ++ B.unpack (encode hoge)
  case hoge of
    SomeResp resp -> do
      case singByProxy resp of
        -- General
        SInitialize -> do
          -- TODO TVarにServerCapabilitiesをセットする
          debugM $ "responseHandler got: " ++ show resp
          debugM "responseHandler: Initialize not implemented"
        SShutdown                        -> notImplemented

        -- Workspace
        SWorkspaceSymbol                 -> notImplemented
        SWorkspaceExecuteCommand         -> notImplemented

        -- Document
        STextDocumentWillSaveWaitUntil   -> notImplemented
        STextDocumentCompletion          -> notImplemented
        SCompletionItemResolve           -> notImplemented
        STextDocumentHover               -> responseHover resp
        STextDocumentSignatureHelp       -> notImplemented
        STextDocumentReferences          -> notImplemented
        STextDocumentDocumentHighlight   -> notImplemented
        STextDocumentDocumentSymbol      -> notImplemented
        STextDocumentFormatting          -> notImplemented
        STextDocumentRangeFormatting     -> notImplemented
        STextDocumentOnTypeFormatting    -> notImplemented
        STextDocumentDefinition          -> notImplemented
        STextDocumentCodeAction          -> notImplemented
        STextDocumentCodeLens            -> notImplemented
        SCodeLensResolve                 -> notImplemented
        STextDocumentDocumentLink        -> notImplemented
        SDocumentLinkResolve             -> notImplemented
        STextDocumentRename              -> notImplemented
        SClientRequestMisc{}             -> notImplemented
    _ -> return ()
  where
    notImplemented :: PluginAction ()
    notImplemented = errorM "responseHandler: not implemented"

responseHover :: ServerResponse 'TextDocumentHoverK -> Neovim PluginEnv () ()
responseHover (Response resp) = do
  debugM $ "responseHover: " ++ show resp
  case resp^. #error of
    Some e -> vim_report_error' (T.unpack (e^. #message))
    None -> case resp^. #result of
      None -> errorM "responseHover: OMG"
      -- TODO highlight #range?
      Some r -> nvimEcho $ pprHoverContents (r^. #contents) ++ "\n"

pprHoverContents :: MarkedString :|: [MarkedString] -> String
pprHoverContents (L ms) = pprMarkedString ms
pprHoverContents (R []) = "hover: no info"
pprHoverContents (R xs) = unlines $ map pprMarkedString xs

pprMarkedString :: MarkedString -> String
pprMarkedString (L s) = s
pprMarkedString (R x) = x^. #value
-- TODO markdownをどう表示するか

