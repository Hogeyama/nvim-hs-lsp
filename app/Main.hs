{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# OPTIONS_GHC -Wall               #-}

module Main where

import           Neovim
import           Neovim.Test_

import           Control.Concurrent                    (killThread, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad                         (forever)

import qualified Data.ByteString.Lazy.Char8            as B

import           Neovim.LSP.Base
import           Neovim.LSP.Util
import           Neovim.LSP.Handler.Notification       (notificationHandler)
import           Neovim.LSP.Handler.Request            (requestHandler)
import           Neovim.LSP.Hoge.Request
import           Neovim.LSP.Hoge.Notification
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type

import           Control.Exception                     (catch)
import           System.Environment                    (unsetEnv)
import           System.Exit                           (ExitCode (..),
                                                        exitSuccess)

print' :: Show a => a -> Neovim r st ()
print' x = liftIO $ print x

main :: IO ()
main = do
  -- これがないとhaskell-ide-engineがghc-8.2.1を使おうとする(?)
  mapM_ unsetEnv [ "GHC_PACKAGE_PATH"
                 --, "HASKELL_DIST_DIR"
                 --, "HASKELL_PACKAGE_SANDBOX"
                 --, "HASKELL_PACKAGE_SANDBOXES"
                 ]

  let hsFile = "test-file/hoge.hs"
      withNeovimEmbedded f m =  testWithEmbeddedNeovim f (Seconds 10) () initialState m
                                  `catch` \ExitSuccess -> return ()

  withNeovimEmbedded (Just hsFile) $ do
        vim_command' "source ./test-file/init.vim"

        initializeLSP "hie" ["--lsp", "-d", "-l", "/tmp/LanguageServer.log"]

        newContext <- liftIO $ newTVarIO initialContext
        -- stateにThreadIDを追加すべき
        (dpth, hths) <- dispatcher newContext
                             [
                             --, handler1
                             --, initializeHandler
                               notificationHandler
                             , requestHandler
                             , handler2
                             ]

        liftIO $ do
          threadDelay $ 10 * 1000 * 1000
          mapM_ killThread $ dpth : hths
          exitSuccess

-- 全部拾うマン
handler1 :: Handler
handler1 = Handler (const True) $
  forever @_ @_ @() $ pull >>= \case
    x -> debugM $ "handler1 got\n" ++ show x

handler2 :: Handler
handler2 = Handler (const False) $ do
  b <- vim_get_current_buffer'

  -- Initialize
  -------------
  let cwd = filePathToUri "/home/hogeyama/.config/nvim/nvim-hs-libs/nvim-hs-lsp/test-file"
      params' = initializeParam Nothing (Just cwd)
  pushRequest @'InitializeK params'


  -- didOpen
  ----------
  didOpenBuffer b
  -- Redundant doと type errorが帰ってくるはず

  -- didChange
  ------------
  liftIO $ threadDelay $ 3 * 1000 * 1000
  nvim_buf_set_lines' b 5 6 False ["  return ()"]
  -- startは含まない, endは含む
  -- 上のは6行目を置換している
  -- start=end=6とすると6,7行目の間に挿入される
  --void $ vim_command ":update" -- とかしない限り実ファイルに影響はない
  --print' =<< getBufContents b -- 確認用
  didChangeBuffer b
  -- Redundant doのみ帰ってくるはず

  -- Hover
  --------
  --let hover = F.textDocument @= textDocumentIdentifier hogeURI
  --         <: F.position     @= (F.line @= 5 <: F.character @= 2 <: nil)
  --         <: nil
  --pushRequest @'TextDocumentHover hover
  hoverRequest b (6,3) -- これで上の代わりになる
  -- line,charは0-indexedでvimのと1ずれる(?)
  -- 次のreturnは range (5,2)~(5,8)
  --   6|  return ()
  --   7|123456789
  -- "return :: () -> IO ()"

  -- Exit
  -------
  liftIO $ threadDelay $ 3 * 1000 * 1000
  -- 実際にやるときはreceiverとかを閉じないといけない
  push exitNotification

-------------------------------------------------------------------------------

pushSonomama :: B.ByteString -> HandlerAction ()
pushSonomama x = do
  outCh <- asks outChan
  liftIO $ atomically $ writeTChan outCh x

hogeURI :: Uri
hogeURI = filePathToUri
  "/home/hogeyama/.config/nvim/nvim-hs-libs/nvim-hs-lsp/test-file/hoge.hs"

