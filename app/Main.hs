{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

module Main where

import           Neovim
import           Neovim.Context                  (quit)
import           Neovim.Test

import           Control.Exception               (catch)
import           Control.Lens                    (use)
import           Control.Lens.Operators
import           Control.Concurrent              (killThread, threadDelay, throwTo)
import           Control.Concurrent.STM
import           Control.Monad                   (forever)
import           Control.Monad.IO.Class          (MonadIO)
import           System.Environment              (unsetEnv)
import           System.Process                  (terminateProcess)
import           System.IO                       (hFlush, hGetLine, stdout)

import           Neovim.LSP.Base
import           Neovim.LSP.Handler.Notification (notificationHandler)
import           Neovim.LSP.Handler.Request      (requestHandler)
import           Neovim.LSP.Hoge.Notification    (didOpenBuffer, didChangeBuffer)
import           Neovim.LSP.Hoge.Request         (hoverRequest)
import           Neovim.LSP.Protocol.Messages    (initializeParam, exitParam)
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util
import           Neovim.LSP.Util as U



print' :: Show a => a -> Neovim r st ()
print' x = liftIO $ print x

main :: IO ()
main = do
  -- これがないとhaskell-ide-engineがghc-8.2.1を使おうとする(?)
  mapM_ unsetEnv [ "GHC_PACKAGE_PATH" ]

  let hsFile = "test-file/hoge.hs"
      withNeovimEmbedded f m =  testWithEmbeddedNeovim f (Seconds 10000) () initialState m

  withNeovimEmbedded (Just hsFile) $ do
        vim_command' "source ./test-file/init.vim"

        initializeLSP "hie" ["--lsp", "-d", "-l", "/tmp/LanguageServer.log"]

        newContext <- liftIO $ newTVarIO initialContext
        ---- stateにThreadIDを追加すべき
        (dpth, hths) <- dispatcher newContext
                             [ notificationHandler
                             , requestHandler
                             , handler2
                             ]

        Just ph' <- fmap (^.ph) <$> use server
        Just herr' <- fmap (^.herr) <$> use server
        threadDelaySec 10
        () <- liftIO $ forever (putStrLn =<< hGetLine herr')
                `catch` \(_::IOError) -> return ()
        liftIO $ do
          hFlush stdout
          mapM_ (throwTo `flip` Blah) $ dpth : hths
          terminateProcess ph'
        threadDelaySec 3
          -- TODO Handlerの終了を待てるようにしたほうがいいのだろうか
          --      Asyncを使ってwaitするのがよいかしら
        return ()

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
  U.pushRequest @'InitializeK params'



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
  hoverRequest b (6,3) -- これで上の代わりになる
  -- line,charは0-indexedでvimのと1ずれる(?)
  -- 次のreturnは range (5,2)~(5,8)
  --   6|  return ()
  --   7|123456789
  -- "return :: () -> IO ()"

  -- Exit
  -------
  threadDelaySec 10
  -- 実際にやるときはreceiverとかを閉じないといけない
  pushNotification @'ExitK exitParam

-------------------------------------------------------------------------------

threadDelaySec :: MonadIO m => Int -> m ()
threadDelaySec n = liftIO $ threadDelay (n * 1000 * 1000)


