
{-# LANGUAGE PartialTypeSignatures #-}

module Neovim.LSP.LspPlugin.Request
  ( requestHandler
  )
  where

import           RIO
import qualified RIO.Map                      as M
import qualified RIO.Text                     as T

import           LSP
import           Util
import           Neovim.User.Choice
import           Neovim.LSP.Base
import           Neovim.LSP.Util              as U

requestHandler :: Worker
requestHandler = Worker "req" requestWorkerAction

requestWorkerAction :: WorkerAction ()
requestWorkerAction = forever $ loggingErrorImmortal $ do
    receiveMessage >>= \case
      SomeReq (req@(Request inner) :: ServerRequest m) -> case singByProxy req of
        SWindowShowMessageRequest -> windowShowMessageRequest req
        SClientRegisterCapability -> sendResponse @m (Just (inner^. #id)) None None
        SClientUnregisterCapability -> sendResponse  @m (Just (inner^. #id)) None None
        SWorkspaceApplyEdit -> respondWorkspaceAplyEdit req
        SServerRequestMisc _ -> logError "requestHandler: Misc: not implemented"
      _ -> return ()

-------------------------------------------------------------------------------
-- WindowShowMessage
-------------------------------------------------------------------------------

windowShowMessageRequest :: Request 'WindowShowMessageRequestK -> WorkerAction ()
windowShowMessageRequest (Request req) = do
    let type'   = req^. #params.__#type
        message = req^. #params.__#message
        actions = req^. #params.__#actions
    caseOfEnumAs type'
       $ (MatchEnum @"error"   $ nvimEchoe $ "LSP: Error: " <> T.unpack message)
      <! (MatchEnum @"warning" $ nvimEchow $ "LSP: Warning: " <> T.unpack message)
      <! (MatchEnum @"info"    $ nvimEchom $ "LSP: Info: " <> T.unpack message)
      <! (MatchEnum @"log"     $ nvimEchom $ "LSP: Log: " <> T.unpack message)
      <! nil
    -- ask action
    mAction <- case actions of
      None -> return Nothing
      Some actions' -> oneOf $ map (view #title) actions'
    let result = fmap (\action -> Record (#title @= action <: nil)) mAction
    sendResponse @'WindowShowMessageRequestK
        (Just (req^. #id))
        (Some result)
        None

-- WorkspaceApplyEdit
---------------------------------------

respondWorkspaceAplyEdit :: Request 'WorkspaceApplyEditK -> WorkerAction ()
respondWorkspaceAplyEdit (Request req) = do
  let edit = req^. #params.__#edit
  logDebug $ displayShow edit
  case edit^. #changes of
    None -> case edit^. #documentChanges of
      None -> logError "respondWorkspaceAplyEdit: Wrong Input!"
      Some cs -> applyDocumentChanges cs >>= ret
    Some cs -> applyChanges cs >>= ret
  where
    ret x = sendResponse @'WorkspaceApplyEditK
              (Just (req^. #id))
              (Some (Record (#applied @= x <! nil)))
              None

applyChanges :: Map Uri [TextEdit] -> WorkerAction Bool
applyChanges cs = do
  mapM_ (uncurry U.applyTextEdit) (M.toList cs)
  return True

-- hieが対応していないので後回しで良い
applyDocumentChanges :: [TextDocumentEdit] -> WorkerAction Bool
applyDocumentChanges _ = logError "WorkspaceApplyEdit: not implemented for versioned changes"
                      >> return False

