
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.Action.Request where


import           RIO
import           RIO.List
import           RIO.List.Partial         (head)
import qualified RIO.Map                  as M
import           RIO.Partial              (fromJust)
import qualified RIO.Text                 as T

import           Control.Lens             (views)
import           Data.Aeson
import           Data.Coerce              (coerce)
import           Data.Extensible.Rexport

import           Data.Either.Combinators  (whenLeft)
import           Neovim                   hiding (Plugin, range, (<>))
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util
import qualified Neovim.User.Choice       as Choice

-------------------------------------------------------------------------------
-- TextDocumentHover {{{
-------------------------------------------------------------------------------

hoverRequest :: (HasOutChan env, HasContext env)
             => Buffer
             -> NvimPos
             -> CallbackOf 'TextDocumentHoverK a
             -> Neovim env (TMVar a)
hoverRequest b p callback = do
  param <- getTextDocumentPositionParams b p
  fromJust <$> pushRequest param (Just callback)

callbackHoverPreview :: CallbackOf 'TextDocumentHoverK ()
callbackHoverPreview (Response resp) = do
  logDebug $ "responseHover: " <> displayShow resp
  void $ withResult resp $ \case
    Nothing -> nvimEcho textDocumentHoverNoInfo
    Just r -> do
      let content = stringOfHoverContents (r^.__#contents)
      writeFileUtf8Builder "/tmp/nvim-hs-lsp.preview" (fromString content)
      vim_command' "pedit /tmp/nvim-hs-lsp.preview"

callbackHover :: CallbackOf 'TextDocumentHoverK ()
callbackHover = callbackHoverWith removeLastNewlines

callbackHoverOneLine :: CallbackOf 'TextDocumentHoverK ()
callbackHoverOneLine = callbackHoverWith $
  head . dropWhile ("```" `isPrefixOf`) . lines

--

callbackHoverWith :: (String -> String) -> CallbackOf 'TextDocumentHoverK ()
callbackHoverWith process (Response resp) = do
  logDebug $ "responseHover: " <> displayShow resp
  void $ withResult resp $ \case
    Nothing -> nvimEcho textDocumentHoverNoInfo
    Just r -> nvimEcho $ process $ stringOfHoverContents (r^.__#contents)

stringOfHoverContents :: MarkedString :|: [MarkedString] :|: MarkupContent -> String
stringOfHoverContents (L ms)     = pprMarkedString ms
stringOfHoverContents (R (L [])) = textDocumentHoverNoInfo
stringOfHoverContents (R (L xs)) = unlines $ map pprMarkedString xs
stringOfHoverContents (R (R x))  = x^.__#value

-- TODO markdownをどう表示するか
pprMarkedString :: MarkedString -> String
pprMarkedString (L s) = s
pprMarkedString (R x) = x^.__#value

removeLastNewlines :: String -> String
removeLastNewlines = reverse . dropWhile (=='\n') .reverse

removeCodeStartEnd :: String -> String
removeCodeStartEnd = unlines . filter ((/="```") . take 3) . lines

textDocumentHoverNoInfo ::  String
textDocumentHoverNoInfo = "textDocument/hover: no info"

--}}}

-------------------------------------------------------------------------------
-- TextDocumentSignatureHelp {{{
-------------------------------------------------------------------------------

signatureHelpRequest :: (HasOutChan env, HasContext env)
                     => Buffer
                     -> NvimPos
                     -> CallbackOf 'TextDocumentSignatureHelpK a
                     -> Neovim env (TMVar a)
signatureHelpRequest b p callback = do
  param <- getTextDocumentPositionParams b p
  fromJust <$> pushRequest param (Just callback)

-- TODO callback

-- }}}

-------------------------------------------------------------------------------
-- TextDocumentDefinition -- {{{
-------------------------------------------------------------------------------

definitionRequest :: (HasOutChan env, HasContext env)
                  => Buffer
                  -> NvimPos
                  -> CallbackOf 'TextDocumentDefinitionK a
                  -> Neovim env (TMVar a)
definitionRequest b p callback = do
  param <- getTextDocumentPositionParams b p
  fromJust <$> pushRequest param (Just callback)

callbackDefinition :: CallbackOf 'TextDocumentDefinitionK ()
callbackDefinition (Response resp) = do
  logDebug $ "responseDefinition: " <> displayShow resp
  void $ withResult resp $ \case
    Nothing -> nvimEcho textDocumentDefinitionNoInfo
    Just [] -> nvimEcho textDocumentDefinitionNoInfo
    Just r  -> jumpToLocation $ head r

jumpToLocation ::  (HasLogFunc env) => Location -> Neovim env ()
jumpToLocation loc = do
  let uri        = loc^.__#uri
      range      = loc^.__#range
      start      = range^.__#start
      (lnum,col) = positionToNvimPos start
  vim_command' "normal! m`"
  m <- vim_command $ unwords
              [ "edit"
              , "+call\\ cursor(" ++ show lnum ++ "," ++ show col ++ ")"
              , uriToFilePath uri
              ]
  whenLeft m (\e -> logError (displayShow e) >> nvimEcho (show e))

textDocumentDefinitionNoInfo ::  String
textDocumentDefinitionNoInfo = "textDocument/definition: no info"

--}}}

-------------------------------------------------------------------------------
-- WorkspaceExecuteCommand {{{
-------------------------------------------------------------------------------
executeCommandRequest :: (HasOutChan env, HasContext env)
                      => String
                      -> Option [Value]
                      -> Maybe (CallbackOf 'WorkspaceExecuteCommandK a)
                      -> Neovim env (Maybe (TMVar a))
executeCommandRequest cmd margs mcallback = do
  let param = Record
            $ #command   @= cmd
           <! #arguments @= margs
           <! nil
  pushRequest param mcallback

--}}}

-------------------------------------------------------------------------------
-- TextDocumentCompletion {{{
-------------------------------------------------------------------------------
completionRequest :: (HasOutChan env, HasContext env)
                  => Buffer
                  -> NvimPos
                  -> CallbackOf 'TextDocumentCompletionK a
                  -> Neovim env (TMVar a)
completionRequest b p callback = do
  uri <- getBufUri b
  let params = Record
             $ #textDocument @= textDocumentIdentifier uri
            <! #position     @= nvimPosToPosition p
            <! #context      @= None
            <! nil
  fromJust <$> pushRequest params (Just callback)

-- TODO error processing
callbackComplete :: CallbackOf 'TextDocumentCompletionK [VimCompleteItem]
callbackComplete (Response resp) = do
  m <- withResult resp $ \case
    Nothing     -> return []
    Just (L cs) -> return $ completeCompletionItems cs
    Just (R cl) -> return $ completeCompletionList cl
  case m of
    Nothing -> return []
    Just xs -> return xs

completeCompletionList :: CompletionList -> [VimCompleteItem]
completeCompletionList cl = completeCompletionItems $ cl^.__#items

completeCompletionItems :: [CompletionItem] -> [VimCompleteItem]
completeCompletionItems cs = map toVimItem cs

toVimItem :: CompletionItem -> VimCompleteItem
toVimItem c
  =  Record
  $  #word      @= c^.__#label
  <! #abbr      @= None
  <! #menu      @= removeNewline <$> c^.__#detail
  <! #info      @= case c^.__#documentation of
                     None            -> None
                     Some (L s)      -> Some s
                     Some (R markup) -> Some (markup^.__#value)
  <! #kind      @= case c^.__#kind of
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
  <! #icase     @= None
  <! #dup       @= Some 1
  <! #empty     @= None
  <! #user_data @= None
  <! nil

removeNewline :: String -> String
removeNewline = concatMap (\c -> if c == '\n' then " ⏐ " else [c])
--removeNewline = map (\c -> if c == '\n' then ' ' else c)
--removeNewline = concatMap (\c -> if c == '\n' then " ␣ " else [c])
-- ⤶ U+2936 ARROW POINTING DOWNWARDS THEN CURVING LEFTWARDS
-- ↵ U+21B5 DOWNWARDS ARROW WITH CORNER LEFTWARDS
-- ⏎ U+23CE RETURN SYMBOL
-- ↲ U+21B2 DOWNWARDS ARROW WITH TIP LEFTWARDS
-- ↩ U+21A9 LEFTWARDS ARROW WITH HOOK
-- ⏐ U+23D0 VERTICAL LINE EXTENSION

--}}}

-------------------------------------------------------------------------------
-- TextDocumentCodeAction {{{
-------------------------------------------------------------------------------

codeAction :: (HasOutChan env, HasContext env)
           => Buffer
           -> (NvimPos, NvimPos)
           -> CallbackOf 'TextDocumentCodeActionK a
           -> Neovim env (TMVar a)
codeAction b (start,end) callback = do
  uri <- getBufUri b
  allDiagnostics <- readContext $
    views (#otherState.diagnosticsMap) (fromMaybe [] . M.lookup uri)
  let params = Record
             $ #textDocument @= textDocumentIdentifier uri
            <! #range        @= Record { fields =
                                  #start @= nvimPosToPosition start
                               <! #end   @= nvimPosToPosition end
                               <! nil }
            <! #context      @= Record context
            <! nil
      context = #diagnostics @= diags
             <! #only @= None
             <! nil
      diags = filter `flip` allDiagnostics $ \diag ->
        let range  = diag^.__#range
            start' = positionToNvimPos $ range^.__#start
            end'   = positionToNvimPos $ range^.__#end
            line   = fst
        in line start' <= line start && line start <= line end'

  fromJust <$> pushRequest params (Just callback)

callbackCodeAction :: CallbackOf 'TextDocumentCodeActionK ()
callbackCodeAction (Response resp) = do
    m <- withResult resp $ \case
      Nothing -> do
        logDebug "callbackCodeAction: got Nothing"
        return ()
      Just xs -> do
        let cmds = lefts $ coerce @_ @[Either _ CodeAction] xs
        case cmds of
          [] -> nvimEchom "no code action"
          [cmd] -> executeCommandOrNot cmd
          _ -> chooseCommandAndExecute cmds
    case m of
      Nothing -> return ()
      Just () -> return ()

chooseCommandAndExecute :: [Command] -> Neovim PluginEnv ()
chooseCommandAndExecute cmds = do
    let titles = map (view #title . fields) cmds
    Choice.oneOf titles >>= \case
      Nothing -> return ()
      Just x -> case find (\cmd -> cmd^.__#title == x) cmds of
        Nothing -> error "impossible"
        Just cmd -> executeCommand cmd

executeCommandOrNot :: Command -> Neovim PluginEnv ()
executeCommandOrNot cmd = do
    b <- Choice.yesOrNo ("execute this command?: " ++ cmd^.__#title)
    when b $ executeCommand cmd

executeCommand :: Command -> Neovim PluginEnv ()
executeCommand cmd = void $ executeCommandRequest (cmd^.__#command) (cmd^.__#arguments) Nothing


-- Util
-------------------------------------------------------------------------------

withResult :: (HasLogFunc env)
           => ResponseMessage a e
           -> (a -> Neovim env ret)
           -> Neovim env (Maybe ret)
withResult resp k =
  case resp^.__#error of
    Some e -> vim_report_error' msg >> return Nothing
      where msg = "nvim-hs-lsp: error from server: " <> T.unpack (e^.__#message)
    None -> case resp^.__#result of
      None   -> logError "withResult: wrong input" >> return Nothing
      Some x -> Just <$> k x

waitCallback :: MonadIO m => m (TMVar a) -> m a
waitCallback m = atomically . takeTMVar =<< m

--}}}

