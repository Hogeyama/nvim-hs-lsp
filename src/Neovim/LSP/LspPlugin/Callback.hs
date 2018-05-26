
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.LspPlugin.Callback where

import           RIO

import           Control.Monad            (forever)
import           Data.Typeable            (cast, typeOf)

import           Neovim                   (vim_report_error')
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
              logDebug $ "run callback function for id: " <> displayShow id'
              x <- callback resp'
              atomically $ putTMVar var x
            Nothing -> do
              logError . fromString $ unlines
                  [ "input type does not match for id: " ++ show id'
                  , "Expected: " ++ show (typeOf expected)
                  , "Actual:   " ++ show (typeOf resp)
                  ]
              vim_report_error' "nvim-hs-lsp: error: callback function type mismatched"
              where expected = let _ = callback expected in expected
        Nothing ->
          logDebug $ "no callback set for id: " <> displayShow id'
    _ -> return ()

