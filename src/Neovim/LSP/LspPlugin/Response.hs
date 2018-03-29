{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.LspPlugin.Response
--  ( responseHandler
--  )
  where

import           Control.Lens
import           Control.Monad              (forever)
import           Data.List                  (intercalate)
import           Data.Extensible

import qualified Data.Text                  as T

import           Text.XFormat.Show          (showf, (%))
import qualified Text.XFormat.Show          as X

import           Neovim                     hiding (Plugin, range)
import           Neovim.LSP.Base
import           Neovim.LSP.Util
import           Neovim.LSP.Protocol.Type

responseHandler :: Plugin
responseHandler = Plugin "resp" responsePluginAction

responsePluginAction :: PluginAction ()
responsePluginAction = forever @_ @() @() $ do
  msg <- pull
  case msg of
    SomeResp resp -> case singByProxy resp of
      -- General
      SInitialize -> do
        -- TODO TVarにServerCapabilitiesをセットする
        debugM $ "responseHandler got: " ++ show resp
        debugM "responseHandler: Initialize not implemented"
      SShutdown                        -> notImplemented

      -- Workspace
      SWorkspaceSymbol                 -> notImplemented
      SWorkspaceExecuteCommand         -> infoM "responseHandler: ignore workspace/executeCommand"

      -- Document
      STextDocumentWillSaveWaitUntil   -> notImplemented
      STextDocumentCompletion          -> complete resp
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

responseHover :: ServerResponse 'TextDocumentHoverK -> Neovim PluginEnv ()
responseHover (Response resp) = do
  debugM $ "responseHover: " ++ show resp
  withResult resp $ \case
    Nothing -> nvimEcho textDocumentHoverNoInfo
    Just r  -> nvimEcho $ removeLastNewlines $
                 pprHoverContents (r^. #contents) ++ "\n"

pprHoverContents :: MarkedString :|: [MarkedString] :|: MarkupContent -> String
pprHoverContents (L ms) = pprMarkedString ms
pprHoverContents (R (L [])) = textDocumentHoverNoInfo
pprHoverContents (R (L xs)) = unlines $ map pprMarkedString xs
pprHoverContents (R (R x))  = x^. #value

-- TODO markdownをどう表示するか
pprMarkedString :: MarkedString -> String
pprMarkedString (L s) = s
pprMarkedString (R x) = x^. #value

removeLastNewlines :: String -> String
removeLastNewlines = reverse . dropWhile (=='\n') .reverse

textDocumentHoverNoInfo ::  String
textDocumentHoverNoInfo = "textDocument/hover: no info"

-------------------------------------------------------------------------------
-- Definitions
-------------------------------------------------------------------------------

responseDefinition :: (HasLoggerName' env)
                   => ServerResponse 'TextDocumentDefinitionK -> Neovim env ()
responseDefinition (Response resp) = do
  debugM $ "responseDefinition: " ++ show resp
  withResult resp $ \case
    Nothing -> nvimEcho textDocumentDefinitionNoInfo
    Just [] -> nvimEcho textDocumentDefinitionNoInfo
    Just r  -> jumpToLocation $ head r

jumpToLocation ::  (HasLoggerName' env) => Location -> Neovim env ()
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

textDocumentDefinitionNoInfo ::  String
textDocumentDefinitionNoInfo = "textDocument/definition: no info"

-------------------------------------------------------------------------------
-- Complete
-------------------------------------------------------------------------------

complete :: (HasLoggerName' env) => ServerResponse 'TextDocumentCompletionK -> Neovim env ()
complete (Response resp) = withResult resp $ \case
  Nothing -> return () -- is it ok?
  Just (L cs) -> completeCompletionItems cs
  Just (R cl) -> completeCompletionList cl

completeCompletionList :: CompletionList -> Neovim env ()
completeCompletionList cl = completeCompletionItems $ cl^. #items

completeCompletionItems :: [CompletionItem] -> Neovim env ()
completeCompletionItems cs = do
  undefined cs

toVimItem :: CompletionItem -> VimCompleteItem
toVimItem c
  =  #word      @= ""
  <: #abbr      @= None
  <: #menu      @= c^. #detail
  <: #info      @= case c^. #documentation of
                     None -> None
                     Some (L s) -> Some s
                     Some (R markup) -> Some (markup^. #value)
  <: #kind      @= case c^. #kind of
                     -- TODO {{{
                     Some  1 -> None     -- text(?)
                     Some  2 -> Some "f" -- method
                     Some  3 -> Some "f" -- function
                     Some  4 -> Some "f" -- constructor
                     Some  5 -> Some "m" -- fields
                     Some  6 -> Some "v" -- variable
                     Some  7 -> Some "v" -- class
                     Some  8 -> Some "v" -- interface
                     Some  9 -> Some "m" -- module
                     Some 10 -> None
                     _       -> None
                     --  v variable
                     --  f function or method
                     --  m member of a struct or class
                     --  t typedef
                     --}}}
  <: #icase     @= None
  <: #dup       @= Some 1
  <: #empty     @= None
  <: #user_data @= None
  <: nil

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

withResult :: (HasLoggerName' env) => ResponseMessage a e -> (a -> Neovim env ()) -> Neovim env ()
withResult resp k =
  case resp^. #error of
    Some e -> vim_report_error' (T.unpack (e^. #message))
    None -> case resp^. #result of
      None -> errorM "withResult: wrong input"
      Some x -> k x

