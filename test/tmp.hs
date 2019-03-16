{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

module Main where

import           Prelude
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

import           LSP
import           Neovim.LSP.Action.Notification    --(didChangeBuffer, didOpenBuffer)
import           Neovim.LSP.Action.Request         --(hoverRequest)
import           Neovim.LSP.Base
import           Neovim.LSP.Plugin
import           Neovim.LSP.LspPlugin.Notification
import           Neovim.LSP.LspPlugin.Request
import           Neovim.LSP.LspPlugin.Callback
import           Neovim.LSP.Util
import           System.Exit (exitSuccess)

main :: IO ()
main = do
  putStrLn ""
  initialEnv <- initialEnvM "/dev/null"
  mapM_ unsetEnv [ "GHC_PACKAGE_PATH" ]
  testWithEmbeddedNeovim Nothing (Seconds 10000) initialEnv $ do
    vim_command "source ./test-file/init.vim"
    vim_command "edit ./test-file/hoge.hs"
    cwd <- getCwd
    startServer "haskell" cwd
      [ notificationHandler, requestHandler, callbackHandler ]
    void $ focusLang "haskell" handler2
    finalizeLSP
    liftIO exitSuccess

handler2 :: Neovim LanguageEnv ()
handler2 = do
  b <- vim_get_current_buffer
  uri <- getBufUri b

  -- didOpen
  ----------
  didOpenBuffer b
  -- Redundant doと type errorが帰ってくるはず

  -- didChange
  ------------
  threadDelaySec 3
  nvim_buf_set_lines b 5 6 False ["  return ()"]
  -- startは含まない, endは含む 上のは6行目を置換している
  -- start=end=6とすると6,7行目の間に挿入される
  --void $ vim_command "update" -- とかしない限り実ファイルに影響はない
  didChangeBuffer b
  -- Redundant doのみ帰ってくるはず

  -- Hover
  --------
  threadDelaySec 3
  waitCallback $ hoverRequest uri (fromNvimPos (6,3)) nopCallback
  -- line,charは0-indexedでvimのと1ずれる(?)
  -- 次のreturnは range (5,2)~(5,8)
  --   6|  return ()
  --   7|123456789
  -- "return :: () -> IO ()"

-------------------------------------------------------------------------------

threadDelaySec :: MonadIO m => Int -> m ()
threadDelaySec n = liftIO $ threadDelay (n * 1000 * 1000)

print' :: Show a => a -> Neovim env ()
print' x = liftIO $ hPutBuilder stdout (getUtf8Builder (displayShow x))

