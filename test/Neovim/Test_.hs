
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Neovim.Test_ where

import           Neovim
import qualified Neovim.Context.Internal                   as Internal
import           Neovim.RPC.Common                         (newRPCConfig)
import           Neovim.RPC.EventHandler                   (runEventHandler)
import           Neovim.RPC.SocketReader                   (runSocketReader)

import           Control.Monad.Reader                      (runReaderT)
import           Control.Monad.Trans.Resource              (runResourceT)
import           System.Exit                               (ExitCode (..))
import           System.Process                            hiding (env)
import           UnliftIO

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
        cleanUp = --do debug "cleanUp"
                     mapM_ cancel [socketReader, eventHandler]
        close   = --do debug "close nvim"
                     liftIO (terminateProcess ph)
    m <- run $
          timeout (fromIntegral sec * 1000 * 1000) $
          finally `flip` cleanUp $
          onException `flip` close $
            action <* async (vim_command "qa!")

    case m of
      Nothing -> fail "testNeovim: timeout"
      Just x  -> waitForProcess ph >>= \case
        ExitFailure i ->
            fail $ "testNeovim: Neovim returned with an exit status of: " ++ show i
        ExitSuccess -> return x

debug :: MonadIO m => String -> m ()
debug = const $ return ()
{-debug s = liftIO $ putStrLn s >> hFlush stdout-}

