
module Neovim.Test.Wrapper
  ( testNeovim
  , Seconds(..)
  ) where

import           RIO
import           Neovim
import           Neovim.Test

data Container a = Container a
  deriving (Show, Typeable)
instance (Show a, Typeable a) => Exception (Container a)

testNeovim :: (Show a, Typeable a)
            => Seconds
            -> env
            -> Neovim env a
            -> IO a
testNeovim sec env action = do
    testWithEmbeddedNeovim Nothing sec env $ do
      x <- action
      throwIO (Container x)
    error "impossible(?)"
  `catch` \(Container x) -> return x

