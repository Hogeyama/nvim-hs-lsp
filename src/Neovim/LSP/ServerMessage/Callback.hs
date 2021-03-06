
{-# OPTIONS_GHC -Wall #-}

module Neovim.LSP.ServerMessage.Callback where

import           RIO

import           Data.Typeable            (cast, typeOf)

import           LSP
import           Util
import           Neovim                   ()
import           Neovim.LSP.Base

callbackHandler :: Worker
callbackHandler = Worker "callback" callbackHandler'

callbackHandler' :: WorkerM ()
callbackHandler' = forever $ loggingErrorImmortal $
    receiveMessage >>= \case
      SomeResp resp@(Response inner) ->
        case inner^. #id of
          Just id' -> getCallbackById id' >>= \case
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
                    vim_report_error
                      "nvim-hs-lsp: error: callback function type mismatched"
                  where expected = let _ = callback expected in expected
            Nothing ->
              logDebug $ "no callback set for id: " <> displayShow id'
          Nothing ->
            logInfo $ "Response without id: " <> displayShow resp
      _ -> return ()

