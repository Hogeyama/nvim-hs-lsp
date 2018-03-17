{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.LspPlugin.Response
--  ( responseHandler
--  )
  where

import           Control.Lens
import           Control.Monad              (forever)
import           Data.List                  (intercalate)
import           Data.Aeson                 as J
import qualified Data.ByteString.Lazy.Char8 as B

import qualified Data.Text                  as T

import           Text.XFormat.Show          (showf, (%))
import qualified Text.XFormat.Show          as X

import           Neovim                     hiding (Plugin, range)
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
        STextDocumentDefinition          -> responseDefinition resp
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

-------------------------------------------------------------------------------
-- Hover
-------------------------------------------------------------------------------

responseHover :: ServerResponse 'TextDocumentHoverK -> Neovim PluginEnv () ()
responseHover (Response resp) = do
  debugM $ "responseHover: " ++ show resp
  case resp^. #error of
    Some e -> vim_report_error' (T.unpack (e^. #message))
    None -> case resp^. #result of
      None -> errorM "responseHover: OMG"
      -- TODO highlight #range?
      Some Nothing -> nvimEcho "hover: no info"
      Some (Just r) -> nvimEcho $ removeLastNewlines $
                          pprHoverContents (r^. #contents) ++ "\n"

pprHoverContents :: MarkedString :|: [MarkedString] -> String
pprHoverContents (L ms) = pprMarkedString ms
pprHoverContents (R []) = "hover: no info"
pprHoverContents (R xs) = unlines $ map pprMarkedString xs

pprMarkedString :: MarkedString -> String
pprMarkedString (L s) = s
pprMarkedString (R x) = x^. #value
-- TODO markdownをどう表示するか

removeLastNewlines :: String -> String
removeLastNewlines = reverse . dropWhile (=='\n') .reverse

-------------------------------------------------------------------------------
-- Definitions
-------------------------------------------------------------------------------

responseDefinition :: ServerResponse 'TextDocumentDefinitionK -> Neovim r st ()
responseDefinition (Response resp) = do
  debugM $ "responseDefinition: " ++ show resp
  case resp^. #error of
    Some e -> vim_report_error' (T.unpack (e^. #message))
    None -> case resp^. #result of
      None -> errorM "responseDefinition: OMG"
      Some Nothing -> nvimEcho "textDocument/definition: no info"
      Some (Just []) -> nvimEcho "textDocument/definition: no info"
      Some (Just r) -> jumpToLocation $ head r
    --                      pprHoverContents (r^. #contents) ++ "\n"

jumpToLocation :: Location -> Neovim r st ()
jumpToLocation loc = do
  let uri   = loc^. #uri
      range = loc^. #range
      start = range^. #start
      cmd   = jumpCommand (uriToFilePath uri) (positionToNvimPos start)
  debugM $ "jumpToLocation: " ++ cmd
  Right{} <- vim_command cmd
  return ()

-- |
-- >>> jumpCommand "/tmp/foo.hs" (15,3)
-- "execute 'normal! m`' | execute 'edit +:call\\ cursor(15,3) /tmp/foo.hs'"
jumpCommand :: FilePath -> NvimPos -> String
jumpCommand file (lnum, col) =
  intercalate " | "
     [ "execute 'normal! m`'"
     , showf ("execute 'edit +:call\\ cursor("%d%","%d%") "%s%"'") lnum col file
     ]
  where d = X.Int; s = X.String

