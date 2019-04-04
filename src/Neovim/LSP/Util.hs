{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall            #-}

module Neovim.LSP.Util where

import           RIO                          hiding ((^.))
import           RIO.Partial                  (read)
import qualified RIO.ByteString               as B
import qualified RIO.List                     as L
import qualified RIO.List.Partial             as L
import qualified RIO.Text                     as T
import qualified RIO.Map                      as M

import           Control.Lens                 ((^.), (.~))
import           Data.Generics.Product        (field)
import           System.IO                    (hGetLine)
import           System.IO.Error              (isEOFError)
import qualified Data.Aeson                   as J
import           Control.Monad.Extra          (whenJustM)
import           System.Process               (CreateProcess (..),
                                               StdStream (..),
                                               createProcess, proc,
                                               terminateProcess,
                                               waitForProcess)
import           Path                         (Path, Abs, Dir,
                                               toFilePath, parseAbsFile,
                                               parseAbsDir)

import           LSP
import           Util
import           Neovim
import           Neovim.LSP.Base
import qualified Neovim.User.Choice       as Choice

-------------------------------------------------------------------------------
-- Neovim
-------------------------------------------------------------------------------

vimCallFunction :: String -> [Object] -> Neovim env Object
vimCallFunction func args = do
  evaluate (rnf func)
  evaluate (rnf args)
  vim_call_function func args

vimCommand :: String -> Neovim env ()
vimCommand s = evaluate (rnf s) >> vim_command s

nvimEcho :: String -> Neovim env ()
nvimEcho s = vimCommand $ "echo " ++ show s

nvimEchom :: String -> Neovim env ()
nvimEchom s = vimCommand $ "echomsg " ++ show s

nvimEchoe :: String -> Neovim env ()
nvimEchoe s =
  vimCommand $ L.intercalate "|"
    [ "echohl ErrorMsg"
    , "echomsg " ++ show s
    , "echohl None"
    ]

nvimEchow :: String -> Neovim env ()
nvimEchow s =
    vimCommand $ L.intercalate "|"
      [ "echohl WarningMsg"
      , "echomsg " ++ show s
      , "echohl None"
      ]

userChoise :: String -> (a -> String) -> [a] -> Neovim env (Maybe a)
userChoise prompt pr = \case
    []  -> return Nothing
    [x] -> Choice.yesOrNo (prompt ++ ": " ++ pr x) >>= \case
      True  -> return (Just x)
      False -> return Nothing
    xs -> do
      let titles = map pr xs
      x <- tryAny $ vimCallFunction "tlib#input#List"
              [ ObjectString "si"
              , ObjectString "choose a codeAction"
              , toObject titles
              ]
      case x of
        Right (fromObject -> Right i) -> return (Just  (xs L.!! (i-1)))
        Left _ -> Choice.oneOf titles >>= \case
            Nothing -> return Nothing
            Just ans -> case L.find (\a -> pr a == ans) xs of
              Nothing -> error "impossible"
              Just x' -> return (Just x')
        _ -> error "どうして"

---

getCwd :: Neovim env (Path Abs Dir)
getCwd = do
  cwd <- fromObject' =<< vimCallFunction "getcwd" []
  case parseAbsDir cwd of
    Nothing -> error $ "fail to parse directory name: " <> cwd
    Just cwd' -> return cwd'

getBufLanguage :: (HasLogFunc env)
               => Buffer -> Neovim env (Maybe String)
getBufLanguage b =
    tryAny (nvim_buf_get_var b "current_syntax" >>= fromObject') >>=
      \case
        Right x -> return (Just x)
        _ -> return Nothing

getBufUri :: Buffer -> Neovim env Uri
getBufUri b = do
    f <- nvim_buf_get_name b
    case parseAbsFile f of
      Nothing -> error $ "fail to parse file name: " <> f
      Just file -> return $ pathToUri file

getNvimPos :: (HasLogFunc env) => Neovim env NvimPos
getNvimPos =
    tryAny (vimCallFunction "getpos" [ObjectString "."] >>= fromObject') >>=
      \case
        Right ([_bufnum, lnum, col, _off] :: [Int]) -> return (lnum,col)
        e -> logError (displayShow e) >> error "getNvimPos"

getBufContents :: Buffer -> Neovim env Text
getBufContents b = T.pack.unlines <$> nvim_buf_get_lines b 0 maxBound False

getCword :: Neovim env String
getCword = fromObject' =<< vim_call_function "expand" [ObjectString "<cword>"]

getCWORD :: Neovim env String
getCWORD = fromObject' =<< vim_call_function "expand" [ObjectString "<cWORD>"]

-------------------------------------------------------------------------------
-- Pos
-------------------------------------------------------------------------------

-- TODO ちゃんとdata型にする
type NvimPos = (Int,Int)

fromNvimPos :: NvimPos -> Position
fromNvimPos (line,char) = Record $
    #line      @= line - 1
 <! #character @= char - 1
 <! nil

toNvimPos :: Position -> NvimPos
toNvimPos pos = (1 + pos^. #line, 1 + pos^. #character)

-------------------------------------------------------------------------------
-- Start Server
-------------------------------------------------------------------------------

startServer
    :: Language       -- ^ Language
    -> Path Abs Dir   -- ^ CWD
    -> [Worker]       -- ^ Worker
    -> NeovimLsp ()
startServer lang cwd {-cmd args-} workers = do
    config <- loadLspConfig
    logDebug $ displayShow config
    let cmd:args = serverCommand config

    -- spawn
    --------
    (Just hin, Just hout, Just herr, ph) <-
       liftIO $ createProcess $ (proc cmd args)
         { cwd = Just (toFilePath cwd)
         , std_in = CreatePipe
         , std_out = CreatePipe
         , std_err = CreatePipe
         }
    liftIO $ do
      hSetBuffering hin  NoBuffering
      hSetBuffering hout NoBuffering
      hSetBuffering herr NoBuffering

    ---- create language env
    ------------------------
    logFunc <- view (field @"logFunc")
    let serverHandles = ServerHandles
          { serverIn = hin
          , serverOut = hout
          , serverErr = herr
          , serverProcHandle = ph
          }
    emptyOtherHandles <- newTVarIO (OtherHandles [])
    inCh <- newTChanIO
    outCh <- newTChanIO
    lspConfig <- loadLspConfig
    newContext <- newTVarIO initialContext
    atomically $ modifyTVar newContext (field @"lspConfig" .~ lspConfig)

    ---- register
    -------------
    let languageEnv = LanguageEnv
                    { logFunc = logFunc
                    , language = lang
                    , serverHandles = serverHandles
                    , otherHandles = emptyOtherHandles
                    , inChan = inCh
                    , outChan = outCh
                    , context = newContext
                    }
    field @"languageMap" %== M.insert lang languageEnv

    -- setup
    --------
    logInfo $ fromString $ unlines
       [ ""
       , "＿人人人人人人＿"
       , "＞　__init__　＜"
       , "￣Y^Y^Y^Y^Y^Y^Y￣"
       ]

    void $ focusLang lang $ do
      registerAsyncHandle "sender" =<< async (sender hin outCh)
      registerAsyncHandle "receiver" =<< async (receiver hout inCh)
      registerAsyncHandle "herr-watcher" =<< async (watcher herr)
      dispatch workers

      ---- communication
      ------------------
      waitCallback $ sendRequest @'InitializeK
         (initializeParam Nothing (Just (pathToUri cwd)))
         nopCallback
      sendNotification @'InitializedK (Record nil)
      whenJustM (getWorkspaceSettings lspConfig) $ \v -> do
        let param = Record (#settings @= v <! nil)
        sendNotification @'WorkspaceDidChangeConfigurationK param
  where
    loadLspConfig = do
        allConfig <- fromObject' =<< vim_get_var "NvimHsLsp_languageConfig"
        let wildConfig = M.findWithDefault M.empty "_" allConfig
            langConfig = M.findWithDefault M.empty (encodeUtf8 lang) allConfig
                         -- langConfig is preferred
            config     = langConfig `M.union` wildConfig
        update config defaultLspConfig
      where
        update :: Map ByteString Object -> LspConfig -> Neovim env LspConfig
        update config =
            update' "autoloadQuickfix"  (field @"autoLoadQuickfix")  >=>
            update' "settingsPath"      (field @"settingsPath")      >=>
            update' "serverCommand"     (field @"serverCommand")     >=>
            update' "formattingOptions" (field @"formattingOptions") >=>
            return
          where
            update' :: NvimObject o
                    => ByteString
                    -> Lens' LspConfig o
                    -> LspConfig
                    -> Neovim env LspConfig
            update' var field' before = case M.lookup var config of
              Just o -> case fromObject o of
                Right new ->
                  return $ before & field' .~ new
                Left{} -> do
                  nvimEchoe $ "invalid config: " <> show var
                  return before
              _ -> return before


    getWorkspaceSettings :: LspConfig -> Neovim env (Maybe J.Value)
    getWorkspaceSettings lspConfig = case lspConfig^.field @"settingsPath" of
        Just file -> liftIO $ J.decodeFileStrict' file
        Nothing -> return Nothing

    watcher :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
            => Handle -> m ()
    watcher herr =
        do s <- B.hGetLine herr
           showAndLog $ "STDERR: " <> displayBytesUtf8 s
      `catchIO` \e ->
        if isEOFError e
        then throwIO e
        else showAndLog $ "STDERR: Exception: " <> displayShow e
      where
        showAndLog msg = do
            logError msg
            --vim_report_error' $ T.unpack $ utf8BuilderToText msg
            -- なんでRIOには'Utf8Builder -> String'の関数がないんだ
            -- rlsがうるさいのでreportやめます

    sender :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
           => Handle -> TChan ByteString -> m ()
    sender serverIn chan = forever $ loggingErrorDeep $ do
        bs <- atomically $ readTChan chan
        hPutBuilder serverIn $ getUtf8Builder $ mconcat
            [ "Content-Length: "
            , displayShow $ B.length bs
            , "\r\n\r\n"
            , displayBytesUtf8 bs
            ]
        logDebug $ "=> " <> displayBytesUtf8 bs


    receiver :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
             => Handle -> TChan ByteString -> m ()
    receiver serverOut chan = forever $ loggingErrorDeep $ do
        v <- receive serverOut
        atomically $ writeTChan chan v
      where
        receive h = do
            len <- liftIO $ contentLength <$> readHeader h
            content <- B.hGet h len
            logDebug $ "<= " <> displayBytesUtf8 content
            return content

        readHeader h = go Header { contentLength = error "broken header"
                                 , contentType   = Nothing }
          where
            go header = do
              x <- hGetLine h
              if | "Content-Length: " `L.isPrefixOf` x ->
                      go $ header { contentLength = read (drop 16 x) }
                 | "Content-Type: " `L.isPrefixOf` x ->
                      go $ header { contentType = Just (drop 14 x) }
                 | "\r" == x -> return header
                 | otherwise -> error $ "receiver: unknown input: " <> show x

data Header = Header
  { contentLength :: ~Int -- lazy initialization
  , contentType   :: Maybe String
  } deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Stop Server
-------------------------------------------------------------------------------

stopServer :: Language -> NeovimLsp ()
stopServer lang = do
    void $ focusLang lang $ do
      sendNotification @'ExitK exitParam
      sh <- view (field @"serverHandles")
      isStillAlive <- fmap isNothing $ timeout (1 * 1000 * 1000) $ do
        logDebug "waiting!"
        liftIO $ waitForProcess (serverProcHandle sh)
      when isStillAlive $
        liftIO $ terminateProcess (serverProcHandle sh)
      mapM_ (cancel.snd) =<< usesTV (field @"otherHandles") unOtherHandles
    field @"languageMap" %== M.delete lang

finalizeLSP :: NeovimLsp ()
finalizeLSP = do
    -- mapM_ stopServer =<< usesTV (field @"languageMap") M.keys
    ls <- usesTV (field @"languageMap") M.keys
    forM_ ls $ \lang -> do
      logDebug $ "finalizeLSP: stopServer: " <> displayShow lang
      stopServer lang

    liftIO =<< view (field @"logFuncFinalizer")
    hClose =<< view (field @"logFileHandle")

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

type QfItem = Record
  '[ "filename" >: Option String
   , "lnum"     >: Option Int
   , "col"      >: Option Int
   , "type"     >: Option String
   , "text"     >: String
   , "valid"    >: Option Bool
   ]

replaceQfList :: HasLogFunc env => [QfItem] -> Neovim env ()
replaceQfList qs = void $ vimCallFunction "setqflist" $! qs +: ['r'] +: []

replaceLocList :: HasLogFunc env => Int -> [QfItem] -> Neovim env ()
replaceLocList winId qs =
    void $ vimCallFunction "setloclist" $! winId +: qs +: ['r'] +: []

diagnosticToQfItems :: Uri -> Diagnostic -> [QfItem]
diagnosticToQfItems uri d
    | ignored = []
    | otherwise = header : rest
  where
    ignored = or
      [ d^. #source == Some "pyflakes"
      , d^. #message == "" -- ghc-modがなんか送ってくる (hie-0.6.0.0)
      , d^. #source == Some "hlint"
          && "Parse error:" `T.isPrefixOf` (d^. #message)
      ]
    header = Record
           $ #filename @= Some (toFilePath (uriToAbsFilePath uri))
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
            Some s -> caseOfEnum s
               $ MatchEnum @"error"       "E"
              <! MatchEnum @"warning"     "W"
              <! MatchEnum @"information" "I"
              <! MatchEnum @"hint"        "I"
              <! nil
            _                 -> "W"
        text = case d^. #source of
            None   -> ""
            Some n -> "[" ++ n ++ "]"
    rest = flip map (T.lines (d^. #message)) $ \msg -> Record
           $ #filename @= None
          <! #lnum     @= None
          <! #col      @= None
          <! #type     @= None
          <! #text     @= T.unpack msg
          <! #valid    @= Some False
          <! nil

locationToQfItem :: Location -> String -> QfItem
locationToQfItem loc text = Record
     $ #filename @= Some filename
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

applyTextEdits :: Uri -> [TextEdit] -> Neovim env ()
applyTextEdits uri edits = do
    text <- fromObject' =<< vimCallFunction "readfile" (uriToFilePath uri+:[])
    let filePath = uriToFilePath uri
        -- NOTE: This sort must be stable.
        edits' = L.reverse $ L.sortOn (view (#range.__#start)) edits
        newText = foldl' applyTextEdit text edits'
    void (vimCallFunction "writefile" (newText +: filePath +: []))
      `catchAny` \e -> nvimEchoe (show e)
    vimCommand $ "edit " ++ filePath

-- TODO これはdoctestに置くべきではない
-- |
-- >>> import Prelude (putStr)
-- >>> :{
--  let text = [ "let () = begin match () with"
--             , "      _ -> ()"
--             , "  end"
--             ]
--      edit = Record
--           $ #range @= Record
--                  ( #start @= Record (#line @= 0 <! #character @= 0 <! nil)
--                 <! #end   @= Record (#line @= 3 <! #character @= 0 <! nil)
--                 <! nil )
--          <! #newText @=
--                  "let () =\n  begin match () with\n    | () -> ()\n  end\n"
--          <! nil
--  in putStr $ T.unpack $ T.unlines $ applyTextEdit text edit
-- :}
-- let () =
--   begin match () with
--     | () -> ()
--   end
--
applyTextEdit :: [Text] -> TextEdit -> [Text]
applyTextEdit text edit =
    let range = edit^. #range
        (before, r)   = L.splitAt (range^.__#start.__#line) text
        (body, after) = L.splitAt (range^.__#end.__#line
                                    - range^.__#start.__#line + 1) r
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

-------------------------------------------------------------------------------
-- WorkspaceEdit
-------------------------------------------------------------------------------

applyWorkspaceEdit :: (HasLogFunc env) => WorkspaceEdit -> Neovim env ()
applyWorkspaceEdit edit = case edit^. #documentChanges of
    Some documentChanges -> applyDocumentChanges documentChanges
    None -> case edit^. #changes of
      Some changes -> applyChanges changes
      None -> return ()

applyChanges :: Map Uri [TextEdit] -> Neovim env ()
applyChanges cs = uncurry applyTextEdits `mapM_` M.toList cs

applyDocumentChanges
  :: (HasLogFunc env)
  => ([TextDocumentEdit]
      :|: [TextDocumentEdit :|: CreateFile :|: RenameFile :|: DeleteFile])
  -> Neovim env ()
applyDocumentChanges = \case
    L xs -> applyTextDocumentEdits xs
    R _  -> logError "Resource operation is not supported"

applyTextDocumentEdits :: HasLogFunc env => [TextDocumentEdit] -> Neovim env ()
applyTextDocumentEdits = mapM_ applyTextDocumentEdit

applyTextDocumentEdit :: HasLogFunc env => TextDocumentEdit -> Neovim env ()
applyTextDocumentEdit textDocumentEdit = do
    let uri   = textDocumentEdit ^. #textDocument.__#uri -- XXX ignore version
        edits = textDocumentEdit ^. #edits
    applyTextEdits uri edits

