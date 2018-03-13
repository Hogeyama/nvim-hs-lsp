{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

module Main where

import           Neovim                            hiding (Plugin)
import           Neovim.Context                    (quit)
import           Neovim.Test

import           Control.Concurrent                (killThread, threadDelay,
                                                    throwTo)
import           Control.Concurrent.STM
import           Control.Exception                 (catch)
import           Control.Lens                      (use)
import           Control.Lens.Operators
import           Control.Monad                     (forever)
import           Control.Monad.IO.Class            (MonadIO)
import           System.Environment                (unsetEnv)
import           System.IO                         (hFlush, hGetLine, stdout)
import           System.Process                    (terminateProcess)

import           Neovim.LSP.Action.Notification    (didChangeBuffer,
                                                    didOpenBuffer)
import           Neovim.LSP.Action.Request         (hoverRequest)
import           Neovim.LSP.Base
import           Neovim.LSP.LspPlugin.Notification (notificationHandler)
import           Neovim.LSP.LspPlugin.Request      (requestHandler)
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

print' :: Show a => a -> Neovim r st ()
print' x = liftIO $ print x

main :: IO ()
main = do
  mapM_ unsetEnv [ "GHC_PACKAGE_PATH" ]
  let f = "test-file/hoge.hs"
  initialEnv <- initialEnvIO
  testWithEmbeddedNeovim (Just f) (Seconds 10000) initialEnv initialState $ do
      vim_command' "source ./test-file/init.vim"

      initializeLsp "hie" ["--lsp", "-d", "-l", "/tmp/LanguageServer.log"]

      -- stateにThreadIDを追加すべき
      (dpth, hths) <- dispatcher
                           [ notificationHandler
                           , requestHandler
                           , handler2
                           ]

      Just ph <- fmap serverPH <$> use server
      Just herr <- fmap serverErr <$> use server
      threadDelaySec 10
      () <- liftIO $ forever (putStrLn =<< hGetLine herr)
              `catch` \(_::IOError) -> return ()
      liftIO $ do
        hFlush stdout
        mapM_ (throwTo `flip` Blah) $ dpth : hths
        terminateProcess ph
      threadDelaySec 3
        -- TODO Handlerの終了を待てるようにしたほうがいいのだろうか
        --      Asyncを使ってwaitするのがよいかしら
      return ()

-- 全部拾うマン
handler1 :: Plugin
handler1 = Plugin (const True) $
  forever @_ @_ @() $ pull >>= \case
    x -> debugM $ "handler1 got\n" ++ show x

handler2 :: Plugin
handler2 = Plugin (const False) $ do
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
  threadDelaySec 3
  nvim_buf_set_lines' b 5 6 False ["  return ()"]
  -- startは含まない, endは含む 上のは6行目を置換している
  -- start=end=6とすると6,7行目の間に挿入される
  --void $ vim_command ":update" -- とかしない限り実ファイルに影響はない
  --print' =<< getBufContents b -- 確認用
  didChangeBuffer b
  -- Redundant doのみ帰ってくるはず

  -- Hover
  --------
  threadDelaySec 3
  hoverRequest b (6,3)
  -- line,charは0-indexedでvimのと1ずれる(?)
  -- 次のreturnは range (5,2)~(5,8)
  --   6|  return ()
  --   7|123456789
  -- "return :: () -> IO ()"

  -- Exit
  -------
  threadDelaySec 10
  --pushNotification @'ExitK exitParam
  -- なんでダメなんだ．文脈から明らかに確定するやろ
  push $ notification @'ExitK exitParam

-------------------------------------------------------------------------------

threadDelaySec :: MonadIO m => Int -> m ()
threadDelaySec n = liftIO $ threadDelay (n * 1000 * 1000)


