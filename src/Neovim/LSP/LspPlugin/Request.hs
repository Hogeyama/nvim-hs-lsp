
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.LspPlugin.Request
  ( requestHandler
  )
  where

import Control.Monad                       (forM_, forever)
import           Control.Lens.Operators
import           Data.Aeson                          as J
import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Lazy.Char8          as B
import           Data.Map                            (Map)
import qualified Data.Map                            as M
import           Data.Extensible

import           Neovim hiding (Plugin, range, err)
import           Neovim.LSP.Base
import           Neovim.LSP.Util
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Protocol.Messages

requestHandler :: Plugin
requestHandler = Plugin requestPred requestPluginAction

requestPred :: InMessage -> Bool
requestPred SomeReq{} = True
requestPred _ = False

requestPluginAction :: PluginAction ()
requestPluginAction = forever @_ @() @() $ do
  SomeReq req <- pull
  debugM $ "requestHandler got " ++ B.unpack (encode req)
  case singByProxy req of
    SWindowShowMessageRequest   -> errorM "requestHandler: not implemented"
      -- これ大変そう
    SClientRegisterCapability   -> errorM "requestHandler: not implemented"
      -- これは対応する必要性が低そう
    SClientUnregisterCapability -> errorM "requestHandler: not implemented"
      -- これも
    SWorkspaceApplyEdit         -> respondWorkspaceAplyEdit req
    SServerRequestMisc _        -> errorM "requestHandler: Misc: not implemented"

-- WorkspaceApplyEdit
---------------------------------------

-- TODO ask whether to apply or not
respondWorkspaceAplyEdit :: Request 'WorkspaceApplyEditK -> PluginAction ()
respondWorkspaceAplyEdit (Request req) = do
  let params = req^. #params
      edit   = params^. #edit :: WorkspaceEdit
  debugM $ show edit
  case edit^. #changes of
    None -> case edit^. #documentChanges of
      None -> errorM "respondWorkspaceAplyEdit: Wrong Input!"
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
    forM_ es $ \e -> do
      let range   = e^. #range
          (l1,_)  = positionToNvimPos (range^. #start)
          (l2,_)  = positionToNvimPos (range^. #end)
          newText = e^. #newText
      m <- do
        vim_command' $ "edit " ++ uriToFilePath uri ++ " | "
                    ++ show l1 ++ "," ++ show (l2-1) ++ "d"
        vim_call_function "setline"
          [ ObjectInt (fromIntegral l1)
          , ObjectArray $ map (ObjectString . BS.pack) (lines newText)
          ]
      case m of
        Right{}  -> return ()
        Left (show->err) -> errorM err >> vim_report_error' err
  return True

-- hieが対応していないので後回しで良い
applyDocumentChanges :: [TextDocumentEdit] -> PluginAction Bool
applyDocumentChanges _ = errorM "WorkspaceApplyEdit: not implemented for versioned changes"
                      >> return False

{-
type WorkspaceEdit = Record
  '[ "changes"         >: Option (Map Uri [TextEdit])
   , "documentChanges" >: Option [TextDocumentEdit]
   -- the latter is preffered.
   -- tell the server whether the client can handle latter in initialize notification.
   ]
type TextDocumentEdit = Record
  '[ "textDocument" >: VersionedTextDocmentIdentifier
   , "edits"        >: [TextEdit]
   ]
type TextEdit = Record
  '[ "range"   >: Range
   , "newText" >: String
   ]
-}


