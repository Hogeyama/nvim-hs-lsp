
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.LspPlugin.Request
  ( requestHandler
  )
  where

import           Control.Lens.Operators
import           Control.Monad                (forM_, forever)
import qualified Data.ByteString.Char8        as BS
import           Data.Extensible
import           Data.Map                     (Map)
import qualified Data.Map                     as M

import           Neovim                       hiding (Plugin, range)
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

requestHandler :: Plugin
requestHandler = Plugin "req " requestPluginAction

requestPluginAction :: PluginAction ()
requestPluginAction = forever @_ @() @() $ do
  msg <- pull
  case msg of
    SomeReq req -> case singByProxy req of
      SWindowShowMessageRequest   -> errorM "requestHandler: not implemented"
        -- これ大変そう
      SClientRegisterCapability   -> errorM "requestHandler: not implemented"
        -- これは対応する必要性が低そう
      SClientUnregisterCapability -> errorM "requestHandler: not implemented"
        -- これも
      SWorkspaceApplyEdit         -> respondWorkspaceAplyEdit req
      SServerRequestMisc _        -> errorM "requestHandler: Misc: not implemented"
    _ -> return ()

-- WorkspaceApplyEdit
---------------------------------------

-- TODO ask whether to apply or not
respondWorkspaceAplyEdit :: Request 'WorkspaceApplyEditK -> PluginAction ()
respondWorkspaceAplyEdit (Request req) = do
  let params = req^. #params
      edit   = params^. #edit
  debugM $ show edit
  case edit^. #changes of
    None -> case edit^. #documentChanges of
      None    -> errorM "respondWorkspaceAplyEdit: Wrong Input!"
      Some cs -> applyDocumentChanges cs >>= ret
    Some cs -> applyChanges cs >>= ret
  where
    ret x = push $ response @'WorkspaceApplyEditK
              (Just (req^. #id)) (Some (result x)) None
    result b = #applied @= b <: nil

-- just apply edits from bottom to top.
-- TODO consider the case of multiple edits with the same start.
applyChanges :: Map Uri [TextEdit] -> PluginAction Bool
applyChanges cs = do
  forM_ (M.toList cs) $ \(uri,es) -> do
    forM_ es $ \e -> catchAndDisplay $ do
      let range   = e^. #range
          (l1,_)  = positionToNvimPos (range^. #start)
          (l2,_)  = positionToNvimPos (range^. #end)
          newText = e^. #newText
      vim_command' $ "edit " ++ uriToFilePath uri ++ " | "
                  ++ show l1 ++ "," ++ show (l2-1) ++ "d"
      void $ vim_call_function' "setline"
        [ ObjectInt (fromIntegral l1)
        , ObjectArray $ map (ObjectString . BS.pack) (lines newText)
        ]
  return True

-- hieが対応していないので後回しで良い
applyDocumentChanges :: [TextDocumentEdit] -> PluginAction Bool
applyDocumentChanges _ = errorM "WorkspaceApplyEdit: not implemented for versioned changes"
                      >> return False

