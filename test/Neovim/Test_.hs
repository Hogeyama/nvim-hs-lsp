
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Neovim.Test_ where

import           RIO

import           Neovim
import qualified Neovim.Context.Internal                   as Internal
import           Neovim.RPC.Common                         (newRPCConfig)
import           Neovim.RPC.EventHandler                   (runEventHandler)
import           Neovim.RPC.SocketReader                   (runSocketReader)

import           Control.Monad.Reader                      (runReaderT)
import           Control.Monad.Trans.Resource              (runResourceT)
import           System.Exit                               (ExitCode (..))
import           System.Process                            hiding (env)

newtype Seconds = Seconds Word

testNeovim :: Seconds
           -> env
           -> Neovim env a
           -> IO a
testNeovim (Seconds sec) env action = do
    -- start up
    -----------
    (Just hin, Just hout, Just _herr, ph) <-
        createProcess (proc "nvim" ["-n","-u","NONE","--embed"])
            { std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    cfg <- Internal.newConfig (pure Nothing) newRPCConfig
    socketReader <- async $ runSocketReader
                    hout
                    (cfg { Internal.pluginSettings = Nothing })
    eventHandler <- async $ runEventHandler
                    hin
                    (cfg { Internal.pluginSettings = Nothing })
    atomically $ putTMVar
                    (Internal.globalFunctionMap cfg)
                    (Internal.mkFunctionMap [])

    -- run Neovim action
    --------------------
    let testCfg = Internal.retypeConfig env cfg
        run a   = runReaderT (runResourceT (Internal.unNeovim a)) testCfg
        cleanUp :: MonadIO m => m ()
        cleanUp = mapM_ cancel [socketReader, eventHandler]
        close :: MonadIO m => m ()
        close   = debug "close" >> liftIO (terminateProcess ph)
    m <- run $
          timeout (fromIntegral sec * 1000 * 1000) $
            onException `flip` close $
              (,) <$> action <*> async (vim_command "qa!")
              -- nvimの正しい殺し方:
              -- + asyncで "qa!"を送る
              --    + "qa!"が成功した場合，async processは残り続ける
              -- + waitForProcessで実際に死んだことを確認する
              -- + asyncをcancelする

    case m of
      Nothing -> fail "testNeovim: timeout"
      Just (x,a) -> do
        exitCode <- waitForProcess' ph
        --exitCode <- waitForProcess  ph -- this hangs. HELPME
        cleanUp >> cancel a
        case exitCode of
          ExitFailure i ->
              fail $ "testNeovim: Neovim returned with an exit status of: " ++ show i
          ExitSuccess -> do
              return x

waitForProcess' :: ProcessHandle -> IO ExitCode
waitForProcess' ph = getProcessExitCode ph >>= \case
  Nothing -> threadDelay (1000 * 100) >> waitForProcess' ph
  Just exitCode -> return exitCode


debug :: MonadIO m => String -> m ()
debug s = liftIO $ hPutBuilder stderr (fromString s)

