
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.Action.Callback where

import           UnliftIO
import           Control.Lens
import           Data.Extensible
import           Data.List                (intercalate)

import qualified Data.Text                as T

import           Text.XFormat.Show        (showf, (%))
import qualified Text.XFormat.Show        as X

import           Neovim                   hiding (Plugin, range)
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

-------------------------------------------------------------------------------
-- Hover
-------------------------------------------------------------------------------

callbackHoverWith :: (String -> String) -> CallbackOf 'TextDocumentHoverK
callbackHoverWith process (Response resp) = do
  debugM $ "responseHover: " ++ show resp
  void $ withResult resp $ \case
    Nothing -> nvimEcho textDocumentHoverNoInfo
    Just r  -> nvimEcho $ process $ stringOfHoverContents (r^. #contents)

callbackHover :: CallbackOf 'TextDocumentHoverK
callbackHover = callbackHoverWith removeLastNewlines

callbackHoverOneLine :: CallbackOf 'TextDocumentHoverK
callbackHoverOneLine = callbackHoverWith $
  head . dropWhile ((=="```") . take 3) . lines


stringOfHoverContents :: MarkedString :|: [MarkedString] :|: MarkupContent -> String
stringOfHoverContents (L ms)     = pprMarkedString ms
stringOfHoverContents (R (L [])) = textDocumentHoverNoInfo
stringOfHoverContents (R (L xs)) = unlines $ map pprMarkedString xs
stringOfHoverContents (R (R x))  = x^. #value

-- TODO markdownをどう表示するか
pprMarkedString :: MarkedString -> String
pprMarkedString (L s) = s
pprMarkedString (R x) = x^. #value

removeLastNewlines :: String -> String
removeLastNewlines = reverse . dropWhile (=='\n') .reverse

removeCodeStartEnd :: String -> String
removeCodeStartEnd = unlines . filter ((/="```") . take 3) . lines

textDocumentHoverNoInfo ::  String
textDocumentHoverNoInfo = "textDocument/hover: no info"

-------------------------------------------------------------------------------
-- Definitions
-------------------------------------------------------------------------------

callbackDefinition :: CallbackOf 'TextDocumentDefinitionK 
callbackDefinition (Response resp) = do
  debugM $ "responseDefinition: " ++ show resp
  void $ withResult resp $ \case
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

-- TODO error processing
callbackComplete :: CallbackBuilder 'TextDocumentCompletionK [VimCompleteItem]
callbackComplete (Response resp) = do
  m <- withResult resp $ \case
    Nothing     -> return []
    Just (L cs) -> return $ completeCompletionItems cs
    Just (R cl) -> return $ completeCompletionList cl
  case m of
    Nothing -> return []
    Just xs -> return xs

completeCompletionList :: CompletionList -> [VimCompleteItem]
completeCompletionList cl = completeCompletionItems $ cl^. #items

completeCompletionItems :: [CompletionItem] -> [VimCompleteItem]
completeCompletionItems cs = map toVimItem cs

toVimItem :: CompletionItem -> VimCompleteItem
toVimItem c
  =  #word      @= c^. #label
  <: #abbr      @= None
  <: #menu      @= removeNewline <$> c^. #detail
  <: #info      @= case c^. #documentation of
                     None            -> None
                     Some (L s)      -> Some s
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

removeNewline :: String -> String
removeNewline = map (\c -> if c == '\n' then ' ' else c)
--substNewline = concatMap (\c -> if c == '\n' then " ␣ " else [c])
-- ⤶ U+2936 ARROW POINTING DOWNWARDS THEN CURVING LEFTWARDS
-- ↵ U+21B5 DOWNWARDS ARROW WITH CORNER LEFTWARDS
-- ⏎ U+23CE RETURN SYMBOL
-- ↲ U+21B2 DOWNWARDS ARROW WITH TIP LEFTWARDS
-- ↩ U+21A9 LEFTWARDS ARROW WITH HOOK

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

withResult :: (HasLoggerName' env)
           => ResponseMessage a e
           -> (a -> Neovim env ret)
           -> Neovim env (Maybe ret)
withResult resp k =
  case resp^. #error of
    Some e -> vim_report_error' (T.unpack (e^. #message)) >> return Nothing
    None -> case resp^. #result of
      None   -> errorM "withResult: wrong input" >> return Nothing
      Some x -> Just <$> k x

type CallbackBuilder m a = ServerResponse m -> PluginAction a

genCallback :: CallbackBuilder m a
            -> Neovim env (TMVar a, CallbackOf m)
genCallback f = do
  var <- newEmptyTMVarIO
  let callback resp = do
        x <- f resp
        atomically $ putTMVar var x
  return (var, callback)

