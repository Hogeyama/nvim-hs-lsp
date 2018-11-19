
module Neovim.Test.Wrapper
  ( testNeovim
  , Seconds(..)
  ) where

import           RIO
import           Neovim
import           Neovim.Test

data EndNeovim = EndNeovim
  deriving (Show, Typeable)
instance Exception EndNeovim

testNeovim :: Seconds
           -> env
           -> Neovim env a
           -> IO a
testNeovim sec env action = do
    var <- newEmptyMVar
    handle (\EndNeovim -> takeMVar var) $ do
      testWithEmbeddedNeovim Nothing sec env $ do
        x <- action
        putMVar var x
        throwIO EndNeovim -- sometimes 'testWithEmbeddedNeovim' blocks forever
                          -- when the action successfully ends.
      error "impossible"

