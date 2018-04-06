{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall            #-}

module Neovim.LSP.Plugin where

import           UnliftIO
import           Control.Lens                      (view)
import Control.Monad                     (unless, void, when)
import           Control.Monad.Extra               (ifM, whenJust)
import           Data.Aeson                        hiding (Object)
import qualified Data.ByteString.Char8             as B
import           Data.Char (isAlphaNum)
import           Data.List (isPrefixOf, partition)
import qualified Data.Map                          as M

import           Neovim                            as Obj

import           Neovim.LSP.Action.Notification
import           Neovim.LSP.Action.Request
import           Neovim.LSP.Action.Callback
import           Neovim.LSP.Base
import           Neovim.LSP.LspPlugin.Callback
import           Neovim.LSP.LspPlugin.Notification (notificationHandler)
import           Neovim.LSP.LspPlugin.Request      (requestHandler)
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

-------------------------------------------------------------------------------
-- Initialize
-------------------------------------------------------------------------------

nvimHsLspInitialize :: CommandArguments -> NeovimLsp ()
nvimHsLspInitialize _ = loggingError $ do
  initialized <- isInitialized
  if initialized then do
    vim_out_write' $ "nvim-hs-lsp: Already initialized" ++ "\n"
  else do
    mft <- getBufLanguage =<< vim_get_current_buffer'
    case mft of
      Just ft -> do
        map' <- errOnInvalidResult $ vim_get_var "NvimHsLsp_serverCommands"
        case M.lookup ft map' of
          Just (cmd:args) -> do
            initializeLsp cmd args
            fileType .== Just ft
            dispatch [notificationHandler, requestHandler, callbackHandler]
            cwd <- filePathToUri <$>
                      errOnInvalidResult (vim_call_function "getcwd" [])
            pushRequest' @'InitializeK (initializeParam Nothing (Just cwd))
            let pat = def { acmdPattern = "*" }
                arg = def { bang = Just True }
            Just Right{} <- addAutocmd "BufRead,BufNewFile"
                              pat (nvimHsLspOpenBuffer arg)
            Just Right{} <- addAutocmd "TextChanged,TextChangedI"
                              pat (nvimHsLspChangeBuffer arg)
            Just Right{} <- addAutocmd "BufWrite"
                              pat (nvimHsLspSaveBuffer arg)
            vim_out_write' $
              "nvim-hs-lsp: Initialized for filetype `" ++ ft ++ "`\n"
            nvimHsLspOpenBuffer def
            void $ vim_command_output "botright copen"
          _ ->
            vim_report_error' $
              "no language server registered for filetype `" ++ ft ++ "`"
      Nothing ->
        vim_report_error'
          "nvim-hs-lsp: Could not initialize: Could not determine the filetype"

whenInitialized' :: Bool -> NeovimLsp () -> NeovimLsp ()
whenInitialized' silent m = isInitialized >>= \case
  True  -> m
  False -> unless silent $
    vim_out_write' $ "nvim-hs-lsp: Not initialized!" ++ "\n"

whenInitialized :: NeovimLsp () -> NeovimLsp ()
whenInitialized = whenInitialized' False

-------------------------------------------------------------------------------
-- Notification
-------------------------------------------------------------------------------

whenAlreadyOpened :: NeovimLsp () -> NeovimLsp ()
whenAlreadyOpened m = do
  uri <- getBufUri =<< vim_get_current_buffer'
  ifM (alreadyOpened uri) m (vim_out_write' "nvim-hs-lsp: Not opened yet\n")

alreadyOpened :: Uri -> NeovimLsp Bool
alreadyOpened uri = M.member uri <$> useTV openedFiles

nvimHsLspOpenBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspOpenBuffer arg = whenInitialized' silent $ do
  b   <- vim_get_current_buffer'
  mft <- getBufLanguage b
  whenJust mft $ \ft -> do
    serverFT <- useTV fileType
    when (Just ft == serverFT) $ do
      uri <- getBufUri b
      unlessM (alreadyOpened uri) $ do
        openedFiles %== M.insert uri 0
        didOpenBuffer b
  where silent = Just True == bang arg

nvimHsLspCloseBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspCloseBuffer _ = whenInitialized $ do
  b <- vim_get_current_buffer'
  uri <- getBufUri b
  ifM (not <$> alreadyOpened uri) (vim_out_write' "nvim-hs-lsp: Not opened yet\n") $ do
    openedFiles %== M.delete uri
    didCloseBuffer b

nvimHsLspChangeBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspChangeBuffer arg = whenInitialized' silent $ whenAlreadyOpened $
  didChangeBuffer =<< vim_get_current_buffer'
  where silent = Just True == bang arg

nvimHsLspSaveBuffer :: CommandArguments -> NeovimLsp ()
nvimHsLspSaveBuffer arg = whenInitialized' silent $ whenAlreadyOpened $
  didSaveBuffer =<< vim_get_current_buffer'
  where silent = Just True == bang arg

nvimHsLspExit :: CommandArguments -> NeovimLsp ()
nvimHsLspExit _ = whenInitialized $ do
  push $ notification @'ExitK exitParam
  finalizeLSP

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

nvimHsLspInfo :: CommandArguments -> NeovimLsp ()
nvimHsLspInfo _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ hoverRequest b pos (Just callbackHoverOneLine)

nvimHsLspHover :: CommandArguments -> NeovimLsp ()
nvimHsLspHover _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ hoverRequest b pos (Just callbackHoverPreview)

nvimHsLspDefinition :: CommandArguments -> NeovimLsp ()
nvimHsLspDefinition _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  pos <- getNvimPos
  void $ definitionRequest b pos (Just callbackDefinition)

-- argument: {file: Uri, start_pos: Position}
nvimHsLspApplyRefactOne :: CommandArguments -> NeovimLsp ()
nvimHsLspApplyRefactOne _ = whenInitialized $ whenAlreadyOpened $ do
  b <- vim_get_current_buffer'
  uri <- getBufUri b
  pos <- getNvimPos
  let  arg = toJSON $ object [ "file" .= uri
                             , "start_pos" .= nvimPosToPosition pos
                             ]
  void $ executeCommandRequest "applyrefact:applyOne" (Some [arg]) Nothing

-------------------------------------------------------------------------------

nvimHsLspComplete :: Object -> String
                  -> NeovimLsp (Either Int [VimCompleteItem])
nvimHsLspComplete findstart base = do
  notInitialized <- not <$> isInitialized
  if notInitialized then
    return (Left (-1))
  else do
    let findStart = case findstart of
          ObjectInt n -> n
          ObjectString s -> read (B.unpack s)
          _ -> error "流石にここに来たら怒っていいよね"
    if findStart == 1 then do
      Left <$> completionFindStart
    else do
      b   <- vim_get_current_buffer'
      pos <- completionCalcPos base
      Just var <- completionRequest b pos (Just callbackComplete)
      xs <- atomically (takeTMVar var)
      let sorted = uncurry (++) $ partition (isPrefixOf base . view #word)  xs
      return (Right sorted)

completionFindStart :: NeovimLsp Int
completionFindStart = do
  s <- nvim_get_current_line'
  col <- snd <$> getNvimPos
  -- TODO use 'iskeyword' of vim?
  let isKeyword c = isAlphaNum c || (c `elem` ['_','\''])
  let foo = reverse $ dropWhile isKeyword $ reverse $ take (col-1) s
  let len = length foo
  debugM $ "COMPLETION: findstart: foo = " ++ show foo
  debugM $ "COMPLETION: findstart: len = " ++ show len
  return len

completionCalcPos :: String -> NeovimLsp NvimPos
completionCalcPos base = do
  (line, col) <- getNvimPos
  debugM $ "COMPLETION: complete: line = " ++ show line
  debugM $ "COMPLETION: complete: col  = " ++ show col
  debugM $ "COMPLETION: complete: base = " ++ show base
  return (line, col+length base)

--

nvimHsLspAsyncComplete :: Int -> Int -> NeovimLsp ()
nvimHsLspAsyncComplete lnum col = do
  initialized <- isInitialized
  if initialized then do
    b <- vim_get_current_buffer'
    s <- nvim_get_current_line'
    debugM $ "COMPLETION: async: col = " ++ show col
    debugM $ "COMPLETION: async: s   = " ++ show (take col s)
    Just var <- completionRequest b (lnum,col) (Just callbackComplete)
    xs <- atomically (takeTMVar var)
    nvim_set_var' nvimHsCompleteResultVar (toObject xs)
  else do
    nvim_set_var' nvimHsCompleteResultVar (toObject ([]::[VimCompleteItem]))

nvimHsCompleteResultVar :: String
nvimHsCompleteResultVar = "NvimHsLspCompleteResult"

