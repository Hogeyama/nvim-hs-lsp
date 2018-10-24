
{-# LANGUAGE PartialTypeSignatures #-}

module Neovim.LSP.LspPlugin.Request
  ( requestHandler
  )
  where

import           RIO
import qualified RIO.Map                      as M
import           RIO.List                     (intercalate)

import           Data.Extensible

import           Neovim                       hiding (Plugin, range)
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

requestHandler :: Plugin
requestHandler = Plugin "req" requestPluginAction

requestPluginAction :: PluginAction ()
requestPluginAction = forever $ loggingErrorImmortal $ do
  msg <- pull
  case msg of
    SomeReq req -> case singByProxy req of
      SWindowShowMessageRequest   -> logError "requestHandler: not implemented"
        -- これ大変そう
      SClientRegisterCapability   -> logError "requestHandler: not implemented"
        -- これは対応する必要性が低そう
      SClientUnregisterCapability -> logError "requestHandler: not implemented"
        -- これも
      SWorkspaceApplyEdit         -> respondWorkspaceAplyEdit req
      SServerRequestMisc _        -> logError "requestHandler: Misc: not implemented"
    _ -> return ()

-- WorkspaceApplyEdit
---------------------------------------

-- TODO ask whether to apply or not
respondWorkspaceAplyEdit :: Request 'WorkspaceApplyEditK -> PluginAction ()
respondWorkspaceAplyEdit (Request req) = do
  let edit = req^.__#params.__#edit
  logDebug $ displayShow edit
  case edit^.__#changes of
    None -> case edit^.__#documentChanges of
      None -> logError "respondWorkspaceAplyEdit: Wrong Input!"
      Some cs -> applyDocumentChanges cs >>= ret
    Some cs -> applyChanges cs >>= ret
  where
    ret x = push $ response @'WorkspaceApplyEditK
              (Just (req^.__#id)) (Some Record { fields = result x }) None
    result b = #applied @= b <! nil

-- just apply edits from bottom to top.
-- TODO consider the case of multiple edits with the same start.
applyChanges :: Map Uri [TextEdit] -> PluginAction Bool
applyChanges cs = do
  forM_ (M.toList cs) $ \(uri,es) -> do
    forM_ es $ \e -> catchAndDisplay $ do
      let range = e^.__#range
          (l1,_)  = positionToNvimPos (range^.__#start)
          (l2,_)  = positionToNvimPos (range^.__#end)
          newText = e^.__#newText
      let cmd1 = "edit " ++ uriToFilePath uri ++ " | "
                  ++ show l1 ++ "," ++ show (l2-1) ++ "d"
          cmd2 = "setline(" ++
                    intercalate ","
                      [ show l1
                      , show $ lines newText
                      ]
                  ++ ")"
      logDebug "applyChanges"
      logDebug $ fromString cmd2
      unless (l1==l2) $ do
        logDebug $ fromString cmd1
        vim_command' $ "edit " ++ uriToFilePath uri ++ " | "
                    ++ show l1 ++ "," ++ show (l2-1) ++ "d"
      void $ vim_call_function_' "setline"
        [ ObjectInt (fromIntegral l1)
        , ObjectArray $
            map (ObjectString . encodeUtf8 . fromString) (lines newText)
        ]
  return True

-- hieが対応していないので後回しで良い
applyDocumentChanges :: [TextDocumentEdit] -> PluginAction Bool
applyDocumentChanges _ = logError "WorkspaceApplyEdit: not implemented for versioned changes"
                      >> return False

