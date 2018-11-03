{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

module Main where

import           RIO
import           Control.Concurrent.STM
import           Control.Lens                      (use)
import           Control.Lens.Operators
import           Control.Monad                     (forever)
import           Control.Monad.IO.Class            (MonadIO)
import           System.Environment                (unsetEnv)
import           System.IO                         (hGetLine, stdout)
import           System.Process                    (terminateProcess)

import           Neovim                            hiding (Plugin, wait)
import           Neovim.Context                    (quit)
import           Neovim.Test

import           Neovim.LSP.Action.Notification    --(didChangeBuffer, didOpenBuffer)
import           Neovim.LSP.Action.Request         --(hoverRequest)
import           Neovim.LSP.Base
import           Neovim.LSP.LspPlugin.Notification
import           Neovim.LSP.LspPlugin.Request
import           Neovim.LSP.LspPlugin.Callback
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util
import           System.Exit (exitSuccess)

main :: IO ()
main = do
  initialEnv <- initialEnvM
  mapM_ unsetEnv [ "GHC_PACKAGE_PATH" ]
  let f = "test-file/hoge.hs"
  testWithEmbeddedNeovim (Just f) (Seconds 10000) initialEnv $ do
    vim_command' "source ./test-file/init.vim"
    initializeLsp "./" "hie" ["--lsp", "-d", "-l", "/tmp/LanguageServer.log"]
    dispatch [ notificationHandler , requestHandler, callbackHandler ]
    wait =<< async handler2
    finalizeLSP
    liftIO exitSuccess

handler2 :: NeovimLsp ()
handler2 = do
  b <- vim_get_current_buffer'

  -- Initialize
  -------------
  let cwd = filePathToUri "/home/hogeyama/.config/nvim/nvim-hs-libs/nvim-hs-lsp/test-file" -- TODO
      params' = initializeParam Nothing (Just cwd)
  pushRequest' @'InitializeK params'

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
  --void $ vim_command "update" -- とかしない限り実ファイルに影響はない
  --print' =<< getBufContents b -- 確認用
  didChangeBuffer b
  -- Redundant doのみ帰ってくるはず

  -- Hover
  --------
  threadDelaySec 3
  waitCallback $ hoverRequest b (6,3) (const (return ()))
  -- line,charは0-indexedでvimのと1ずれる(?)
  -- 次のreturnは range (5,2)~(5,8)
  --   6|  return ()
  --   7|123456789
  -- "return :: () -> IO ()"

  -- Exit
  -------
  print' ()
  threadDelaySec 3
  pushNotification @'ExitK exitParam

-------------------------------------------------------------------------------

threadDelaySec :: MonadIO m => Int -> m ()
threadDelaySec n = liftIO $ threadDelay (n * 1000 * 1000)

print' :: Show a => a -> Neovim env ()
print' x = liftIO $ hPutBuilder stdout (getUtf8Builder (displayShow x))

