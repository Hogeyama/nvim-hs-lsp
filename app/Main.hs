
module Main where

import           RIO
import           Neovim
import qualified Neovim.LSP as LSP

main :: IO ()
main = neovim defaultConfig { plugins = [ LSP.plugin ] }
