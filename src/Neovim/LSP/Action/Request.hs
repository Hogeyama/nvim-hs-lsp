
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Neovim.LSP.Action.Request where


import           RIO
import           RIO.List
import           RIO.List.Partial         (head)
import qualified RIO.Map                  as M

import           Control.Lens             (views)
import           Data.Aeson
import           Data.Coerce              (coerce)
import           Data.Extensible.Rexport
import           Data.Generics.Product    (field)
import           Data.Either.Combinators  (whenLeft)

import           Neovim                   hiding (Plugin, range, (<>))
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util
import qualified Neovim.User.Choice       as Choice

-------------------------------------------------------------------------------
-- アレ
-------------------------------------------------------------------------------

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
  sendRequest param callback

callbackHoverPreview :: CallbackOf 'TextDocumentHoverK ()
callbackHoverPreview (Response resp) = do
  logDebug $ "responseHover: " <> displayShow resp
  void $ withResponse resp $ \case
    Nothing -> nvimEcho textDocumentHoverNoInfo
    Just r -> do
      let content = stringOfHoverContents (r^. #contents)
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
  void $ withResponse resp $ \case
    Nothing -> nvimEcho textDocumentHoverNoInfo
    Just r -> nvimEcho $ process $ stringOfHoverContents (r^. #contents)

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
  sendRequest param callback

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
  sendRequest param callback

callbackDefinition :: CallbackOf 'TextDocumentDefinitionK ()
callbackDefinition (Response resp) = do
  logDebug $ "responseDefinition: " <> displayShow resp
  void $ withResponse resp $ \case
    Nothing -> nvimEcho textDocumentDefinitionNoInfo
    Just [] -> nvimEcho textDocumentDefinitionNoInfo
    Just r  -> jumpToLocation $ head r

jumpToLocation ::  (HasLogFunc env) => Location -> Neovim env ()
jumpToLocation loc = do
  let uri        = loc^. #uri
      range      = loc^. #range
      start      = range^. #start
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
  case mcallback of
    Just callback -> Just <$> sendRequest param callback
    Nothing -> do sendRequest' @'WorkspaceExecuteCommandK param
                  return Nothing

--}}}

-------------------------------------------------------------------------------
-- WorkspaceSymbol {{{
-------------------------------------------------------------------------------
workspaceSymbol
  :: (HasOutChan env, HasContext env)
  => String
  -> CallbackOf 'WorkspaceSymbolK a
  -> Neovim env (TMVar a)
workspaceSymbol sym callback =
    sendRequest (Record (#query @= sym <! nil)) callback

callbackWorkspaceSymbol :: CallbackOf 'WorkspaceSymbolK ()
callbackWorkspaceSymbol (Response resp) = void $ withResponse resp $ \case
    Nothing -> nvimEchom "workspace/Symbol: no symbols"
    Just symbolInfos -> do
      logInfo $ "workspace/Symbol: " <> displayShow symbolInfos
      replaceLocList 0 $ map symbolInfomartionToQfItem symbolInfos
      unless (null symbolInfos) (vim_command' "copen")
  where
    symbolInfomartionToQfItem symInfo =
        locationToQfItem (symInfo^. #location) (symInfo^. #name)
          -- TODO 他の情報

--type SymbolInformation = Record
--  '[ "name" >: String
--   , "kind" >: Number
--   , "deprecated" >: Option Bool
--   , "location" >: Location
--   , "containerName" >: Option String
--   ]

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
  sendRequest params callback

-- TODO error processing
callbackComplete :: CallbackOf 'TextDocumentCompletionK [VimCompleteItem]
callbackComplete (Response resp) = do
  m <- withResponse resp $ \case
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
  =  Record
  $  #word      @= c^. #label
  <! #abbr      @= None
  <! #menu      @= removeNewline <$> c^. #detail
  <! #info      @= case c^. #documentation of
                     None            -> None
                     Some (L s)      -> Some s
                     Some (R markup) -> Some (markup^. #value)
  <! #kind      @= case c^. #kind of
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
    views (field @"diagnosticsMap") (fromMaybe [] . M.lookup uri)
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
        let range  = diag^. #range
            start' = positionToNvimPos $ range^. #start
            end'   = positionToNvimPos $ range^. #end
            line   = fst
        in line start' <= line start && line start <= line end'

  sendRequest params callback

callbackCodeAction :: CallbackOf 'TextDocumentCodeActionK ()
callbackCodeAction (Response resp) = void $ withResponse resp $ \case
    Nothing -> do
      logDebug "callbackCodeAction: got Nothing"
      return ()
    Just xs -> do
      let cmds = lefts $ coerce @_ @[Either _ CodeAction] xs
      case cmds of
        [] -> nvimEchom "no code action"
        [cmd] -> executeCommandOrNot cmd
        _ -> chooseCommandAndExecute cmds

chooseCommandAndExecute :: (HasOutChan env, HasContext env) => [Command] -> Neovim env ()
chooseCommandAndExecute cmds = do
    let titles = map (view #title . fields) cmds
    Choice.oneOf titles >>= \case
      Nothing -> return ()
      Just x -> case find (\cmd -> cmd^. #title == x) cmds of
        Nothing -> error "impossible"
        Just cmd -> executeCommand cmd

executeCommandOrNot :: (HasOutChan env, HasContext env) => Command -> Neovim env ()
executeCommandOrNot cmd = do
    b <- Choice.yesOrNo ("execute this command?: " ++ cmd^. #title)
    when b $ executeCommand cmd

executeCommand :: (HasOutChan env, HasContext env) => Command -> Neovim env ()
executeCommand cmd = void $ executeCommandRequest (cmd^. #command) (cmd^. #arguments) (Just nopCallback)

--}}}

-------------------------------------------------------------------------------
-- TextDocumentFormatting {{{
-------------------------------------------------------------------------------

textDocumentFormatting
  :: (HasOutChan env, HasContext env, HasLogFunc env)
  => Buffer
  -> FormattingOptions
  -> Neovim env (TMVar ())
textDocumentFormatting b fopts = do
    uri <- getBufUri b
    let param = Record
              $ #textDocument @= Record (#uri @= uri <! nil)
             <! #options @= fopts
             <! nil
    sendRequest param (callbackTextDocumentFormatting uri)

textDocumentRangeFormatting
  :: (HasOutChan env, HasContext env, HasLogFunc env)
  => Buffer
  -> (NvimPos, NvimPos)
  -> FormattingOptions
  -> Neovim env (TMVar ())
textDocumentRangeFormatting b (start,end) fopts = do
    uri <- getBufUri b
    let param = Record
              $ #textDocument @= Record (#uri @= uri <! nil)
             <! #range @= range
             <! #options @= fopts
             <! nil
        range = Record
              $ #start @= nvimPosToPosition start
             <! #end @= nvimPosToPosition end
             <! nil
    sendRequest param (callbackTextDocumentRangeFormatting uri)

callbackTextDocumentFormatting :: Uri -> CallbackOf 'TextDocumentFormattingK ()
callbackTextDocumentFormatting uri (Response resp) = callbackTextEdits uri resp

callbackTextDocumentRangeFormatting :: Uri -> CallbackOf 'TextDocumentRangeFormattingK ()
callbackTextDocumentRangeFormatting uri (Response resp) = callbackTextEdits uri resp

callbackTextEdits
  :: Show e
  => Uri
  -> ResponseMessage (Nullable [TextEdit]) e
  -> Neovim WorkerEnv ()
callbackTextEdits uri resp =
    void $ withResponse resp $ \case
      Nothing -> return ()
      Just edits -> applyTextEdit uri edits

--}}}

-------------------------------------------------------------------------------
-- TextDocumentReferences {{{
-------------------------------------------------------------------------------

textDocumentReferences
  :: (HasOutChan env, HasContext env)
  => Buffer
  -> NvimPos
  -> CallbackOf 'TextDocumentReferencesK a
  -> Neovim env (TMVar a)
textDocumentReferences b p callback = do
    pos <- getTextDocumentPositionParams b p
    let param = Record
              $ fields pos `happend`
                (#context @= context <! nil)
        context = Record
                $ #includeDeclaration @= True
               <! nil
    sendRequest param callback

callbackTextDocumentReferences :: CallbackOf 'TextDocumentReferencesK ()
callbackTextDocumentReferences (Response resp) = void $ withResponse resp $ \case
    Nothing -> nvimEchom "textDocument/references: No result"
    Just locs -> do
      logInfo $ "textDocument/references: " <> displayShow locs
      replaceLocList 0 =<< mapM locationToQfItem' locs -- TODO set winId (current win is used when 0 is set)
      unless (null locs) $ vim_command' "botright lopen"
  where
    locationToQfItem' loc = do
        Just text <- fmap lastMaybe $ errOnInvalidResult $
                        vimCallFunction "readfile" (filename +: False +: lnum +: [])
        return $ locationToQfItem loc text
      where
        filename = uriToFilePath (loc^. #uri)
        range = loc^. #range
        start = range^. #start
        lnum = 1 + start^. #line

--}}}

-------------------------------------------------------------------------------
-- TextDocumentDocumentSymbol {{{
-------------------------------------------------------------------------------

textDocumentDocumentSymbol
  :: (HasOutChan env, HasContext env)
  => Buffer
  -> Neovim env (TMVar ())
textDocumentDocumentSymbol b = do
    uri <- getBufUri b
    let params = Record
               $ #textDocument @= textDocumentIdentifier uri
              <! nil
    sendRequest params  (callbackTextDocumentDocumentSymbol uri)

callbackTextDocumentDocumentSymbol :: Uri -> CallbackOf 'TextDocumentDocumentSymbolK ()
callbackTextDocumentDocumentSymbol uri (Response resp) = void $ withResponse resp $ \case
    Nothing -> nvimEchom "textDocument/documentSymbol: no symbols"
    Just (L docSyms) -> do
      logInfo $ "textDocument/documentSymbol: " <> displayShow docSyms
      replaceLocList 0 $ map documentSymbolToQfItem docSyms
      unless (null docSyms) $ vim_command' "botright lopen"
    Just (R symInfos) -> do
      logInfo $ "textDocument/documentSymbol: " <> displayShow symInfos
      replaceLocList 0 $ map symbolInfomartionToQfItem symInfos
      unless (null symInfos) $ vim_command' "botright lopen"
  where
    documentSymbolToQfItem (DocumentSymbol docSym) = Record
         $ #filename @= Some filename
        <! #lnum     @= Some lnum
        <! #col      @= Some col
        <! #type     @= Some "I" -- TODO
        <! #text     @= text
        <! #valid    @= Some True
        <! nil
      where
        filename = uriToFilePath uri
        range = docSym^. #range
        start = range^. #start
        lnum = 1 + start^. #line
        col  = 1 + start^. #character
        text = docSym^. #name -- TODO other info
    symbolInfomartionToQfItem symInfo = -- TODO other info
        locationToQfItem (symInfo^. #location) (symInfo^. #name)

--newtype DocumentSymbol = DocumentSymbol (Record
--  '[ "name" >: String
--   , "detail" >: Option String
--   , "kind" >: SymbolKind
--   , "deprecated" >: Option Bool
--   , "range" >: Range
--   , "children" >: Option [DocumentSymbol]
--   ])

--}}}

