
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Neovim.LSP.ClientMessage.NotificationSpec (spec) where

import           Prelude
import           RIO
import qualified RIO.ByteString                        as B
import qualified RIO.List                              as L
import qualified RIO.List.Partial                      as L
import qualified RIO.Map                               as M
import           RIO.Partial                           (read)
import qualified RIO.Text                              as T

import           Control.Lens                          ((.~))
import           Data.Aeson
import           Data.Generics.Product                 (HasField' (..), field)
import           Neovim.Context.Internal
import           Neovim.Test.Wrapper
import           Path
import           Test.Hspec

import           LSP
import           Neovim
import           Neovim.Debug
import           Neovim.LSP.Base
import           Neovim.LSP.Plugin
import           Neovim.LSP.ClientMessage.Notification
import           Neovim.LSP.ClientMessage.Request
import           Neovim.LSP.ServerMessage.Callback
import           Neovim.LSP.Util
import           System.Directory                      (getCurrentDirectory,
                                                        removeDirectoryRecursive)
import           Util

spec :: Spec
spec = do
  ctx <- runIO $ newTVarIO initialContext
  outChan <- runIO newTChanIO
  inChan <- runIO newTChanIO

  let shouldReturn' m x = do
        r <- debug (WorkerEnv inChan outChan ctx mempty) m
        case r of
          Right ret -> ret `shouldBe` x
          Left e    -> error (show e)

  describe "textDocument/hover" $ do
    describe "callback" $ do
      let mkResp contents =
            Response $ Record
               $ #jsonrpc @= "2.0"
              <! #id      @= Just (IDNum 0.0)
              <! #result  @= Some (Just $ Record (
                                 #contents @= contents
                              <! #range    @= None
                              <! nil))
              <! #error   @= None
              <! nil
          resp1 = mkResp $ L . L $ "hogehoge"
          resp2 = mkResp $ L . R $ Record $
                     #language @= "haskell"
                  <! #value    @= "hogehoge"
                  <! nil
          resp3 = mkResp $ R . L $
                  [ R . Record $
                       #language @= "haskell"
                    <! #value    @= "hoge"
                    <! nil
                  , L "fuga"
                  ]
          resp4 = mkResp $ R . R $ Record
                   $ #kind  @= #plaintext
                  <! #value @= "hogehoge"
                  <! nil
      specify "1" $
        callbackHoverAux return resp1 `shouldReturn'` Just (Right "hogehoge")
      specify "2" $
        callbackHoverAux return resp2 `shouldReturn'` Just (Right "hogehoge")
      specify "3" $
        callbackHoverAux return resp3 `shouldReturn'` Just (Right "hoge\nfuga\n")
      specify "4" $
        callbackHoverAux return resp4 `shouldReturn'` Just (Right "hogehoge")



