
module Main where

import           RIO
import           Neovim
import qualified Neovim.LSP as LSP

main :: IO ()
main = do
    neovim defaultConfig
      { plugins = [ LSP.plugin ]
      , logOptions = Just ("/tmp/nvim-hs.log", ERROR)
      }
