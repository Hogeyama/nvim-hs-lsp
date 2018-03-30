
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.LspPlugin.Callback where

import           Control.Lens
import           Control.Monad            (forever)
import           Data.Typeable            (cast, typeOf)
import           UnliftIO

import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type

callbackHandler :: Plugin
callbackHandler = Plugin "callback" callbackPluginAction

callbackPluginAction :: PluginAction ()
callbackPluginAction = do
  forever $ pull >>= \case
    SomeResp resp@(Response (view #id -> Just id')) ->
      getCallback id' >>= \case
        Just (Callback var callback) -> do
          removeCallback id'
          case cast resp of
            Just resp' -> do
              debugM $ "run callback function for id: " ++ show id'
              x <- callback resp'
              atomically $ putTMVar var x
            Nothing -> do
              errorM $ unlines
                  [ "input type does not match for id: " ++ show id'
                  , "Expected: " ++ show (typeOf expected)
                  , "Actual:   " ++ show (typeOf resp)
                  ]
              where expected = let x = undefined; _ = callback expected in x
        Nothing ->
          debugM $ "no callback set for id: " ++ show id'
    _ -> return ()

