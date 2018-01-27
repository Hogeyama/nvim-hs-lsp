
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Neovim.Test_
  ( testWithEmbeddedNeovim
  , Seconds(..)
  ) where

import           Neovim
import qualified Neovim.Context.Internal      as Internal
import           Neovim.RPC.Common            (newRPCConfig, RPCConfig)
import           Neovim.RPC.EventHandler      (runEventHandler)
import           Neovim.RPC.SocketReader      (runSocketReader)

import           Control.Concurrent
import           Control.Concurrent.STM       (atomically, putTMVar)
import           Control.Exception.Lifted
import           Control.Monad.Reader         (runReaderT)
import           Control.Monad.State          (runStateT)
import           Control.Monad.Trans.Resource (runResourceT)
import           GHC.IO.Exception             (ioe_filename)
import           System.Directory
import           System.Exit                  (ExitCode (..))
import           System.IO                    (Handle, hSetBuffering,BufferMode(..))
import           System.Process
import           Text.PrettyPrint.ANSI.Leijen (red, text, putDoc, (<$$>))


-- | Type synonym for 'Word'.
newtype Seconds = Seconds Word


-- | Run the given 'Neovim' action according to the given parameters.
-- The embedded neovim instance is started without a config (i.e. it is passed
-- @-u NONE@).
--
-- If you want to run your tests purely from haskell, you have to setup
-- the desired state of neovim with the help of the functions in
-- "Neovim.API.String".
testWithEmbeddedNeovim
    :: Maybe FilePath -- ^ Optional path to a file that should be opened
    -> Seconds        -- ^ Maximum time (in seconds) that a test is allowed to run
    -> r              -- ^ Read-only configuration
    -> st             -- ^ State
    -> Neovim r st a  -- ^ Test case
    -> IO ()
testWithEmbeddedNeovim file timeout r st (Internal.Neovim a) =
    runTest `catch` catchIfNvimIsNotOnPath
  where
    runTest = do
        (_, _, ph, cfg) <- startEmbeddedNvim file timeout

        let testCfg = Internal.retypeConfig r st cfg

        void $ runReaderT (runStateT (runResourceT a) st) testCfg

        -- vim_command isn't asynchronous, so we need to avoid waiting for the
        -- result of the operation since neovim cannot send a result if it
        -- has quit.
        let Internal.Neovim q = vim_command "qa!"
        void . forkIO . void $ runReaderT (runStateT (runResourceT q) st ) testCfg

        waitForProcess ph >>= \case
            ExitFailure i ->
                fail $ "Neovim returned with an exit status of: " ++ show i

            ExitSuccess ->
                return ()


catchIfNvimIsNotOnPath :: IOException -> IO ()
catchIfNvimIsNotOnPath e = case ioe_filename e of
    Just "nvim" ->
        putDoc . red $ text "The neovim executable 'nvim' is not on the PATH."
                    <$$> text "You may not be testing fully!"

    _           ->
        throw e

startEmbeddedNvim
    :: Maybe FilePath
    -> Seconds
    -> IO (Handle, Handle, ProcessHandle, Internal.Config RPCConfig st)
startEmbeddedNvim file (Seconds timeout) = do
    args <- case file of
                Nothing ->
                    return []

                Just f -> do
                    -- 'fail' should work with most testing frameworks. In case
                    -- it doesn't, please file a bug report!
                    unlessM (doesFileExist f) . fail $ "File not found: " ++ f
                    return [f]

    (Just hin, Just hout, _, ph) <-
        createProcess (proc "nvim" (["-n","-u","NONE","--embed"] ++ args))
            { std_in = CreatePipe
            , std_out = CreatePipe
            }
    hSetBuffering hin  NoBuffering

    -- TODO これすると error, called at src/Neovim/LSP/Handler.hs:167:27
    --      になる
    hSetBuffering hout NoBuffering

    cfg <- Internal.newConfig (pure Nothing) newRPCConfig

    void . forkIO $ runSocketReader
                    hout
                    (cfg { Internal.pluginSettings = Nothing })

    void . forkIO $ runEventHandler
                    hin
                    (cfg { Internal.pluginSettings = Nothing })

    atomically $ putTMVar
                    (Internal.globalFunctionMap cfg)
                    (Internal.mkFunctionMap [])

    void . forkIO $ do
        threadDelay $ fromIntegral timeout * 1000 * 1000
        getProcessExitCode ph >>= maybe (terminateProcess ph) (\_ -> return ())

    return (hin, hout, ph, cfg)

