{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
{-# OPTIONS_GHC -Wall                  #-}

module Neovim.LSP.Handler.Response
  ( responseHandler
  )
  where

import           Control.Monad                       (forever)
import           Data.Aeson                          as J
import qualified Data.ByteString.Lazy.Char8          as B

import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Type

responseHandler :: Handler
responseHandler = Handler responsePred responseHandlerAction

responsePred :: InMessage -> Bool
responsePred SomeReq{} = True
responsePred _ = False

responseHandlerAction :: HandlerAction ()
responseHandlerAction = forever @_ @() @() $ do
    SomeRes res <- pull
    debugM $ "responseHandler got " ++ B.unpack (encode res)
    case singByProxy res of
      -- General
      SInitialize -> do
        -- TODO TVarにServerCapabilitiesをセットする
        debugM $ "responseHandler got: " ++ show res
        debugM "responseHandler: Initialize not implemented"
      SShutdown                        -> notImplemented

      -- Workspace
      SWorkspaceSymbol                 -> notImplemented
      SWorkspaceExecuteCommand         -> notImplemented

      -- Document
      STextDocumentWillSaveWaitUntil   -> notImplemented
      STextDocumentCompletion          -> notImplemented
      SCompletionItemResolve           -> notImplemented
      STextDocumentHover               -> notImplemented
      STextDocumentSignatureHelp       -> notImplemented
      STextDocumentReferences          -> notImplemented
      STextDocumentDocumentHighlight   -> notImplemented
      STextDocumentDocumentSymbol      -> notImplemented
      STextDocumentFormatting          -> notImplemented
      STextDocumentRangeFormatting     -> notImplemented
      STextDocumentOnTypeFormatting    -> notImplemented
      STextDocumentDefinition          -> notImplemented
      STextDocumentCodeAction          -> notImplemented
      STextDocumentCodeLens            -> notImplemented
      SCodeLensResolve                 -> notImplemented
      STextDocumentDocumentLink        -> notImplemented
      SDocumentLinkResolve             -> notImplemented
      STextDocumentRename              -> notImplemented
      SClientRequestMisc{}             -> notImplemented
  where
    notImplemented :: HandlerAction ()
    notImplemented = errorM "responseHandler: not implemented"


