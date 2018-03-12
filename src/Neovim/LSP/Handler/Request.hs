
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.Handler.Request
  ( requestHandler
  )
  where

import           Control.Monad                       (forever)
import           Data.Aeson                          as J
import qualified Data.ByteString.Lazy.Char8          as B

import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type

requestHandler :: Handler
requestHandler = Handler requestPred requestHandlerAction

requestPred :: InMessage -> Bool
requestPred SomeReq{} = True
requestPred _ = False

requestHandlerAction :: HandlerAction ()
requestHandlerAction = forever @_ @() @() $ do
  SomeReq req <- pull
  debugM $ "requestHandler got " ++ B.unpack (encode req)
  case singByProxy req of
    SWindowShowMessageRequest   -> do
      -- これ大変そう
      errorM "requestHandler: not implemented"
    SClientRegisterCapability   -> do
      -- これは対応する必要性が低そう
      errorM "requestHandler: not implemented"
    SClientUnregisterCapability -> do
      -- これも
      errorM "requestHandler: not implemented"
    SWorkspaceApplyEdit         -> do
      -- これはとても面倒
      errorM "requestHandler: not implemented"
    SServerRequestMisc _ -> do
      errorM "requestHandler: Misc: not implemented"

