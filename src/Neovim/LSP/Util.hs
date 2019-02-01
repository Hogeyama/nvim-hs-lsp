
{-# OPTIONS_GHC -Wall            #-}

module Neovim.LSP.Util where

import           RIO                          hiding ((^.))
import qualified RIO.List                     as L
import qualified RIO.List.Partial             as L
import qualified RIO.Text                     as T
import qualified RIO.Map                      as M

import           Control.Lens                 ((^.))
import           Data.Extensible.Rexport

import           Neovim
--import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type

-------------------------------------------------------------------------------

vimCallFunction :: String -> [Object] -> Neovim env (Either NeovimException Object)
vimCallFunction func args = do
  func' <- evaluate (force func)
  args' <- evaluate (force args)
  vim_call_function func' args'

vimCallFunction' :: String -> [Object] -> Neovim env Object
vimCallFunction' func args = do
  func' <- evaluate (force func)
  args' <- evaluate (force args)
  vim_call_function' func' args'

nvimEcho :: String -> Neovim env ()
nvimEcho s = vim_command' $ "echo " ++ show s

nvimEchom :: String -> Neovim env ()
nvimEchom s = vim_command' $ "echomsg " ++ show s

nvimEchoe :: String -> Neovim env ()
nvimEchoe s =
    vim_command' $ L.intercalate "|"
      [ "echohl ErrorMsg"
      , "echomsg " ++ show s
      , "echohl None"
      ]

nvimEchow :: String -> Neovim env ()
nvimEchow s =
    vim_command' $ L.intercalate "|"
      [ "echohl WarningMsg"
      , "echomsg " ++ show s
      , "echohl None"
      ]

-------------------------------------------------------------------------------

getBufLanguage :: (HasLogFunc env)
               => Buffer -> Neovim env (Maybe String)
getBufLanguage b = nvim_buf_get_var b "current_syntax" >>= \case
    Right (fromObject -> Right x) -> return (Just x)
    _ -> return Nothing

getBufUri :: Buffer -> Neovim env Uri
getBufUri b = filePathToUri <$> nvim_buf_get_name' b

getNvimPos :: (HasLogFunc env) => Neovim env NvimPos
getNvimPos = vimCallFunction "getpos" [ObjectString "."] >>= \case
  Right (fromObject -> Right [_bufnum, lnum, col, _off]) -> return (lnum,col)
  e -> logError (displayShow e) >> error "getNvimPos"

getBufContents :: Buffer -> Neovim env Text
getBufContents b = T.pack.unlines <$> nvim_buf_get_lines' b 0 maxBound False

getTextDocumentPositionParams :: Buffer -> NvimPos
                              -> Neovim env TextDocumentPositionParams
getTextDocumentPositionParams b p = do
  uri <- getBufUri b
  let pos = Record
          $ #textDocument @= textDocumentIdentifier uri
         <! #position     @= nvimPosToPosition p
         <! nil
  return pos

-------------------------------------------------------------------------------
-- Pos
-------------------------------------------------------------------------------

-- TODO ちゃんとdata型にする
type NvimPos = (Int,Int)

nvimPosToPosition :: NvimPos -> Position
nvimPosToPosition (line,char) = Record $
    #line      @= line - 1
 <! #character @= char - 1
 <! nil

positionToNvimPos :: Position -> NvimPos
positionToNvimPos pos = (1 + pos^. #line, 1 + pos^. #character)

-------------------------------------------------------------------------------
-- Completion
-------------------------------------------------------------------------------

type VimCompleteItem = Record
  '[ "word"      >: String        -- the text that will be inserted
   , "abbr"      >: Option String -- abbreviation of "word"
   , "menu"      >: Option String -- extra text for the popup menu, displayed after "word" or "abbr"
   , "info"      >: Option String -- more information about the item, can be displayed in a preview window
   , "kind"      >: Option String -- single letter indicating the type of completion
   , "icase"     >: Option Int    -- ignore case (when zero or omitted, case sensitive)
   , "dup"       >: Option Int    -- non-zero if the same name is used
   , "empty"     >: Option Int
   , "user_data" >: Option String -- TODO Value is not an instance of NvimObject
   ]

-------------------------------------------------------------------------------
-- Diagnostics
-------------------------------------------------------------------------------

diagnosticsToQfItems :: Uri -> Map Uri [Diagnostic] -> [QfItem]
diagnosticsToQfItems prior diagMap = pripri ++ rest
  where
    pripri = toQfItems prior (M.findWithDefault [] prior diagMap)
    rest   = M.foldMapWithKey toQfItems (M.delete prior diagMap)
    toQfItems uri diagnostics =
        concatMap (diagnosticToQfItems uri) $
          L.sortOn (view #severity) diagnostics

-------------------------------------------------------------------------------

type QfItem = OrigRecord
  '[ "filename" >: Option String
   , "lnum"     >: Option Int
   , "col"      >: Option Int
   , "type"     >: Option String
   , "text"     >: String
   , "valid"    >: Option Bool
   ]

replaceQfList :: HasLogFunc env => [QfItem] -> Neovim env ()
replaceQfList qs = void $ vimCallFunction' "setqflist" $! qs +: ['r'] +: []

replaceLocList :: HasLogFunc env => Int -> [QfItem] -> Neovim env ()
replaceLocList winId qs = void $ vimCallFunction' "setloclist" $! winId +: qs +: ['r'] +: []

diagnosticToQfItems :: Uri -> Diagnostic -> [QfItem]
diagnosticToQfItems uri d
    | d^. #source == Some "pyflakes" = [] -- temporal measure for pyls
    | otherwise = header : rest
  where
    header = #filename @= Some (uriToFilePath uri)
          <! #lnum     @= Some lnum
          <! #col      @= Some col
          <! #type     @= Some errorType
          <! #text     @= text
          <! #valid    @= Some True
          <! nil
      where
        start = d^. #range.__#start
        lnum = 1 + start^. #line
        col  = 1 + start^. #character
        errorType = case d^. #severity of
            Some Error        -> "E"
            Some Warning      -> "W"
            Some Information  -> "I"
            Some Hint         -> "I"
            _                 -> "W"
        text = case d^. #source of
            None   -> ""
            Some n -> "[" ++ n ++ "]"
    rest = flip map (T.lines (d^. #message)) $ \msg ->
             #filename @= None
          <! #lnum     @= None
          <! #col      @= None
          <! #type     @= None
          <! #text     @= T.unpack msg
          <! #valid    @= Some False
          <! nil

locationToQfItem :: Location -> String -> QfItem
locationToQfItem loc text =
       #filename @= Some filename
    <! #lnum     @= Some lnum
    <! #col      @= Some col
    <! #type     @= Some "I"
    <! #text     @= text
    <! #valid    @= Some True
    <! nil
  where
    filename = uriToFilePath (loc^. #uri)
    range = loc^. #range
    start = range^. #start
    lnum = 1 + start^. #line
    col  = 1 + start^. #character

-------------------------------------------------------------------------------
-- TextEdit
-------------------------------------------------------------------------------

applyTextEdit :: Uri -> [TextEdit] -> Neovim env ()
applyTextEdit uri edits = do
    text <- errOnInvalidResult $ vimCallFunction "readfile" (uriToFilePath uri+:[])
    let filePath = uriToFilePath uri
        -- NOTE: This sort must be stable.
        edits' = L.reverse $ L.sortOn (view (#range.__#start)) edits
        newText = foldl' applyTextEditOne text edits'
    void (vimCallFunction "writefile" (newText +: filePath +: []))
      `catchAny` \e -> nvimEchoe (show e)
    vim_command' $ "edit " ++ filePath

-- TODO これはdoctestに置くべきではない
-- |
-- >>> import Prelude (putStr)
-- >>> :{
--  let text = [ "let () = begin match () with"
--             , "      _ -> ()"
--             , "  end"
--             ]
--      edit = Record
--           $ #range @= Record ( #start @= Record (#line @= 0 <! #character @= 0 <! nil)
--                             <! #end   @= Record (#line @= 3 <! #character @= 0 <! nil)
--                             <! nil )
--          <! #newText @= "let () =\n  begin match () with\n    | () -> ()\n  end\n"
--          <! nil
--  in putStr $ T.unpack $ T.unlines $ applyTextEditOne text edit
-- :}
-- let () =
--   begin match () with
--     | () -> ()
--   end
--
applyTextEditOne :: [Text] -> TextEdit -> [Text]
applyTextEditOne text edit =
    let range = edit^. #range
        (before, r)   = L.splitAt (range^. #start.__#line) text
        (body, after) = L.splitAt (range^. #end.__#line - range^.__#start.__#line + 1) r
        body' = T.lines $ b <> edit^. #newText <> a
          where
            b = T.take (range^. #start.__#character) (L.head body)
            a = if length text > range^. #end.__#line
                then T.drop (range^. #end.__#character) (L.last body)
                else ""
    in before <> body' <> after

--`TextEdit[]`
-- Complex text manipulations are described with an array of TextEdit’s, representing a single change to the document.
--
-- All text edits ranges refer to positions in the __original document__.
-- Text edits ranges must never overlap, that means no part of the original document must be manipulated by more than one edit.
-- However, it is possible that multiple edits have the same start position:
-- multiple inserts, or any number of inserts followed by a single remove or replace edit.
-- If multiple inserts have the same position, the order in the array defines the order in which the inserted strings appear in the resulting text.

