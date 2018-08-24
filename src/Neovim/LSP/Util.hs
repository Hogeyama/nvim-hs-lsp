
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall            #-}

module Neovim.LSP.Util where

import           RIO                          hiding ((^.))
import           RIO.List                     (sortBy)
import qualified RIO.Text                     as T
import qualified RIO.Map                      as M

import           Control.Lens                 ((^.))
import           Data.Extensible
import           Data.Singletons

import           Neovim
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type

getCWD :: Neovim env Uri
getCWD = filePathToUri <$> errOnInvalidResult (vim_call_function "getcwd" [])

getBufLanguage :: (HasLogFunc env)
               => Buffer -> Neovim env (Maybe String)
getBufLanguage b = nvim_buf_get_var b "current_syntax" >>= \case
    Right (fromObject -> Right x) ->
      return (Just x)
    _ -> return Nothing

getBufUri :: Buffer -> Neovim env Uri
getBufUri b = filePathToUri <$> nvim_buf_get_name' b

getNvimPos :: (HasLogFunc env) => Neovim env NvimPos
getNvimPos = vim_call_function "getpos" [ObjectString "."] >>= \case
  Right (fromObject -> Right [_bufnum, lnum, col, _off]) -> return (lnum,col)
  e -> logError (displayShow e) >> error "getNvimPos"

getBufContents :: Buffer -> Neovim env Text
getBufContents b = T.pack.unlines <$> nvim_buf_get_lines' b 0 maxBound False

getTextDocumentPositionParams :: Buffer -> NvimPos
                              -> Neovim env TextDocumentPositionParams
getTextDocumentPositionParams b p = do
  uri <- getBufUri b
  let pos = #textDocument @= textDocumentIdentifier uri
         <! #position     @= nvimPosToPosition p
         <! nil
  return pos

catchAndDisplay :: (HasLogFunc env) => Neovim env () -> Neovim env ()
catchAndDisplay = handleAny $ \e -> logError (displayShow e) >> vim_report_error' (show e)

-------------------------------------------------------------------------------
-- TODO To be moved
-------------------------------------------------------------------------------

-- この関数どこに置こうか

-- | Because @m@ is not uniquely determined by type @RequestParam 'Client m@,
--   type annotation is always required when you use this function.
--   The GHC extension @-XTypeApplication@ is useful to do this.
--
-- > pushRequest @'InitializeK (initializeParam Nothing Nothing)
--
-- >>> :set -XTypeApplications -XDataKinds
-- >>> let wellTyped _ = "OK"
-- >>> wellTyped $ pushRequest @'InitializeK @LspEnv (initializeParam Nothing Nothing)
-- "OK"
--
-- TODO これをユーザーに見せるのはどうなのか．でもtype checkはして欲しいしなあ
pushRequest :: forall (m :: ClientRequestMethodK) env a.
               (ImplRequest m, HasOutChan env, HasContext env)
            => RequestParam m
            -> Maybe (CallbackOf m a)
            -> Neovim env (Maybe (TMVar a))
pushRequest param mcallback = do
    let method = fromSing (sing :: Sing m)
    id' <- genUniqueID
    addIdMethodMap id' method
    push $ request @m id' param
    case mcallback of
      Just callback -> do
        var <- newEmptyTMVarIO
        registerCallback id' $ Callback var callback
        return (Just var)
      Nothing -> return Nothing

pushRequest' :: forall (m :: ClientRequestMethodK) env
             .  (ImplRequest m, HasOutChan env, HasContext env)
             => RequestParam m
             -> Neovim env ()
pushRequest' param = void $ pushRequest @m param Nothing

pushNotification :: forall (m :: ClientNotificationMethodK) env
                 .  (ImplNotification m, HasOutChan env)
                 => NotificationParam m -> Neovim env ()
pushNotification param = push $ notification @m param

-- TODO NeovimのPos,RangeとLSPのPos,Rangeを上手く変換する
type NvimPos = (Int,Int)

nvimPosToPosition :: NvimPos -> Position
nvimPosToPosition (line,char) =
    #line      @= line - 1
 <! #character @= char - 1
 <! nil

positionToNvimPos :: Position -> NvimPos
positionToNvimPos pos = (1 + pos^. #line, 1 + pos^. #character)

nvimEcho :: String -> Neovim env ()
nvimEcho s = vim_command' $ "echo " ++ show s

nvimEchom :: String -> Neovim env ()
nvimEchom s = vim_command' $ "echom " ++ show s

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
          sortBy (compare `on` view #severity) diagnostics

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
          <! #lnum     @= Some lnum
          <! #col      @= Some col
          <! #type     @= Some errorType
          <! #text     @= text
          <! #valid    @= Some True
          <! nil
      where
        start = d^. (#range :: FieldOptic "range") . #start
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

