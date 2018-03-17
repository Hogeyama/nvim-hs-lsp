
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall         #-}

module Neovim.LSP.Action.Request
  --(
  --)
  where

import           Neovim
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util

-- TextDocumentHover Request
---------------------------------------
hoverRequest :: (HasOutChannel r, HasContext r)
             => Buffer -> NvimPos -> Neovim r st ()
hoverRequest b p = pushRequest @'TextDocumentHoverK
  =<< getTextDocumentPositionParams b p

-- TextDocumentSignatureHelp Request
---------------------------------------
signatureHelpRequest :: (HasOutChannel r, HasContext r)
                     => Buffer -> NvimPos -> Neovim r st ()
signatureHelpRequest b p = pushRequest @'TextDocumentSignatureHelpK
  =<< getTextDocumentPositionParams b p

-- TextDocumentDefinition Request
---------------------------------------
definitionRequest :: (HasOutChannel r, HasContext r)
                  => Buffer -> NvimPos -> Neovim r st ()
definitionRequest b p = pushRequest @'TextDocumentDefinitionK
  =<< getTextDocumentPositionParams b p


