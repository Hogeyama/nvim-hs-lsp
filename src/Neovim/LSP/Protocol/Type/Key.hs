
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Neovim.LSP.Protocol.Type.Key where

import           Data.Extensible (mkField)

-- OverloadedLabelsはチェインさせると型推論に失敗するので
-- 一応mkFieldして使えるようにしておく．
-- 今の所使わずに済んでいる．

$(concat <$> mapM mkField
  [ "jsonrpc"
  , "id"
  , "method"
  , "params"
  , "code"
  , "message"
  , "result"
  , "error"
  , "line"
  , "character"
  , "start"
  , "end"
  , "uri"
  , "range"
  , "severity"
  , "source"
  , "title"
  , "command"
  , "arguments"
  , "newText"
  , "changes"
  , "version"
  , "languageId"
  , "text"
  , "textDocument"
  , "position"
  , "processId"
  , "rootPath"
  , "rootUri"
  , "initializationOptions"
  , "capabilities"
  , "retry"
  , "resolveProvider"
  , "triggerCharacters"
  , "firstTriggerCharacter"
  , "moreTriggerCharacter"
  , "textDocumentSync"
  , "hoverProvider"
  , "completionProvider"
  , "signatureHelpProvider"
  , "definitionProvider"
  , "referencesProvider"
  , "documentHighlightProvider"
  , "documentSymbolProvider"
  , "workspaceSymbolProvider"
  , "codeActionProvider"
  , "codeLensProvider"
  , "documentFormattingProvider"
  , "documentRangeFormattingProvider"
  , "documentOnTypeFormattingProvider"
  , "renameProvider"
  , "documentChanges"
  , "language"
  , "scheme"
  , "pattern"
  , "trace"
  , "workspace"
  , "experimental"
  , "diagnostics"
  , "contentChanges"
  , "rangeLength"
  ])

