
{-# LANGUAGE PartialTypeSignatures #-}

module Neovim.LSP.LspPlugin.Request
  ( requestHandler
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Aeson                    as J

import           LSP
import           Util
import           Neovim.User.Choice
import           Neovim.LSP.Base
import           Neovim.LSP.Util               as U

requestHandler :: Worker
requestHandler = Worker "req" requestWorkerAction

requestWorkerAction :: WorkerAction ()
requestWorkerAction = forever $ loggingErrorImmortal $ do
  receiveMessage >>= \case
    SomeReq (req@(Request inner) :: ServerRequest m) -> case singByProxy req of
      SWindowShowMessageRequest -> windowShowMessageRequest req
      SClientRegisterCapability ->
        sendResponse @m (Just (inner ^. #id)) None None
      SClientUnregisterCapability ->
        sendResponse @m (Just (inner ^. #id)) None None
      SWorkspaceApplyEdit -> respondWorkspaceAplyEdit req
      SWorkspaceFolders ->
        sendResponse @m (Just (inner ^. #id)) (Some Nothing) None
      SWorkspaceConfiguration -> sendResponse @m (Just (inner ^. #id))
                                                 (Some nulls)
                                                 None
        where nulls = replicate (length (inner ^. #params . __ #items)) J.Null
      SServerRequestMisc _ -> logError "requestHandler: Misc: not implemented"
    _ -> return ()

-------------------------------------------------------------------------------
-- WindowShowMessage
-------------------------------------------------------------------------------

windowShowMessageRequest
  :: Request 'WindowShowMessageRequestK -> WorkerAction ()
windowShowMessageRequest (Request req) = do
  let type'   = req ^. #params . __ #type
      message = req ^. #params . __ #message
      actions = req ^. #params . __ #actions
  caseOfEnum type'
    $  MatchEnum @"error"   (nvimEchoe $ "LSP: Error: "   <> T.unpack message)
    <! MatchEnum @"warning" (nvimEchow $ "LSP: Warning: " <> T.unpack message)
    <! MatchEnum @"info"    (nvimEchom $ "LSP: Info: "    <> T.unpack message)
    <! MatchEnum @"log"     (nvimEchom $ "LSP: Log: "     <> T.unpack message)
    <! nil
  -- ask action
  mAction <- case actions of
    None          -> return Nothing
    Some actions' -> oneOf $ map (view #title) actions'
  let result = fmap (\action -> Record (#title @= action <: nil)) mAction
  sendResponse @ 'WindowShowMessageRequestK (Just (req ^. #id))
                                            (Some result)
                                            None

-- WorkspaceApplyEdit
---------------------------------------

respondWorkspaceAplyEdit :: Request 'WorkspaceApplyEditK -> WorkerAction ()
respondWorkspaceAplyEdit (Request req) = do
  let edit = req ^. #params . __ #edit
  logDebug $ displayShow edit
  applyWorkspaceEdit edit
  let applied = True
  sendResponse @ 'WorkspaceApplyEditK
      (Just (req ^. #id))
      (Some (Record (#applied @= applied <! nil)))
      None

