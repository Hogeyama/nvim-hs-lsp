
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# OPTIONS_GHC -Wall            #-}

module Neovim.LSP.Util where

import           Data.Extensible
import           Data.Singletons
import           Data.Text                    (Text)
import qualified Data.Text                    as T

import           Neovim
import           Neovim.LSP.Base
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type

--  testWithEmbeddedNeovimを使うとb:current_syntaxが存在しないことになっててダメ
--  (filetypeは存在するのになんでだろう)
--  extensionから推測するのが妥当か?
getBufLanguage :: Buffer -> Neovim r st String
getBufLanguage b = nvim_buf_get_var b "current_syntax" >>= \case
    Right o | Right s <- fromObject o ->
        return s
    Left e -> do
        errorM $ unlines
          [ "nvim_buf_get_var current_syntax: " ++ show e
          , "use default value \"haskell\""]
        return "haskell"
    _  -> error "didOpenCurrentBuffer: impossible?"

getBufUri :: Buffer -> Neovim r st Uri
getBufUri b = filePathToUri <$> nvim_buf_get_name' b

getBufContents :: Buffer -> Neovim r st Text
getBufContents b = T.pack.unlines <$> nvim_buf_get_lines' b 0 maxBound False

getTextDocumentPositionParams :: Buffer -> NvimPos
                              -> Neovim r st TextDocumentPositionParams
getTextDocumentPositionParams b p = do
  uri <- getBufUri b
  let pos = #textDocument @= textDocumentIdentifier uri
         <: #position     @= nvimPosToPosition p
         <: nil
  return pos


-------------------------------------------------------------------------------
-- TODO To be moved
-------------------------------------------------------------------------------

-- この関数どこに置こうか

-- | Because @m@ is not uniquely determined by type @RequestParam 'Client m@,
--   type annotation is always required when you use this function.
--   The GHC extension @-XTypeApplication@ is useful to do this.
--
-- > pushRequest @'InitializeK (initializeParam Nothing Nothing)
--
-- >>> :set -XTypeApplications -XDataKinds
-- >>> let wellTyped _ = "OK"
-- >>> wellTyped $ pushRequest @'InitializeK @LspEnv (initializeParam Nothing Nothing)
-- "OK"
--
-- TODO これをユーザーに見せるのはどうなのか．でもtype checkはして欲しいしなあ
pushRequest :: forall (m :: ClientRequestMethodK) r st
            .  (ImplRequest m, HasOutChannel r, HasContext r)
            => RequestParam m -> Neovim r st ()
pushRequest param = do
    let method = fromSing (sing :: Sing m)
    id' <- genUniqueID
    addIdMethodMap id' method
    push $ request @m id' param

pushNotification :: forall (m :: ClientNotificationMethodK) r st
                 .  (ImplNotification m, HasOutChannel r)
                 => NotificationParam m -> Neovim PluginEnv st ()
pushNotification param = push $ notification @m param

-- TODO NeovimのPos,RangeとLSPのPos,Rangeを上手く変換する
type NvimPos = (Int,Int)

nvimPosToPosition :: NvimPos -> Position
nvimPosToPosition (line,char) =
    #line      @= line - 1
 <: #character @= char - 1
 <: nil

positionToNvimPos :: Position -> NvimPos
positionToNvimPos pos = (1 + pos^. #line, 1 + pos^. #character)

nvimEcho :: String -> Neovim r st ()
nvimEcho s = vim_command' $ "echo " ++ show s

nvimEchom :: String -> Neovim r st ()
nvimEchom s = vim_command' $ "echom " ++ show s


