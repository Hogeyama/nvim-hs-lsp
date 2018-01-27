
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall         #-}

module Neovim.LSP.Hoge.Request
  --(
  --)
  where

import           Neovim
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

-- TextDocumentHover Request
---------------------------------------
hoverRequest :: Buffer -> NvimPos -> Neovim HandlerConfig st ()
hoverRequest b p = pushRequest @'TextDocumentHoverK
  =<< getTextDocumentPositionParams b p

-- TextDocumentSignatureHelp Request
---------------------------------------
signatureHelpRequest :: Buffer -> NvimPos -> Neovim HandlerConfig st ()
signatureHelpRequest b p = pushRequest @'TextDocumentSignatureHelpK
  =<< getTextDocumentPositionParams b p

