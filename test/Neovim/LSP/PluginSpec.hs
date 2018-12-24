
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Neovim.LSP.PluginSpec where

import           RIO
import           RIO.List.Partial                  (tail)
import           Test.Hspec
import           Data.Extensible
import           Neovim.Test.Wrapper
import           Neovim.Context.Internal

import           Neovim

import           Neovim.LSP.Action.Notification
import           Neovim.LSP.Action.Request
import           Neovim.LSP.Base
import           Neovim.LSP.Plugin
import           Neovim.LSP.LspPlugin.Callback     (callbackHandler)
import           Neovim.LSP.Protocol.Messages
import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Util
import           System.Directory                  (getCurrentDirectory, removeDirectoryRecursive)

neovimEnv :: IO (Config LspEnv)
neovimEnv = newConfig (return Nothing) initialEnvM

testSpec :: IO ()
testSpec = hspec spec

spec :: Spec
spec = do
  baseDirectory <- runIO getCurrentDirectory
  let removeStackWorkDir =
        runIO $ void $ tryIO $ removeDirectoryRecursive "./test-file/.stack-work"

  describe "completion" $ do
    specify "findStart simple" $ do
      let curLine = "  Foo.hoge  "
          ---------            ^
          col     = 11
          base    = "hoge"
      completionFindStart curLine col `shouldBe` 6
      completionPos base (1,6) `shouldBe` (1,10)
      let curLine = "  Foo.hoge  "
          ---------             ^
          col     = 12
      completionFindStart curLine col `shouldBe` 11
    specify "findStart underscore" $ do
      let curLine = "  Foo.name_with_underscore"
          col     = 20
      completionFindStart curLine col `shouldBe` 6
    specify "findStart quote" $ do
      let curLine = "  Foo.name'with'quote"
          col     = 20
      completionFindStart curLine col `shouldBe` 6

  describe "definition" $ do
    removeStackWorkDir
    specify "simple" $ do
      let src = "./test-file/Definition.hs"
          definition1 = testNeovimLsp (Seconds 10) src $ do
              threadDelaySec 1 -- wait for loading
              b <- vim_get_current_buffer'
              waitCallback $ definitionRequest b (8,11) return
          expected = Response . Record
              $  #jsonrpc @= "2.0"
              <: #id @= Just (IDNum 1.0)
              <: #result @= Some (Just
                    [ Record $
                       #uri @= filePathToUri (baseDirectory ++ tail src)
                    <: #range @= Record { fields =
                                  #start @= Record (#line @= 11 <: #character @= 0 <: nil)
                               <: #end   @= Record (#line @= 11 <: #character @= 4 <: nil)
                               <: nil }
                    <: nil])
              <: #error @= None <: nil
      definition1 `shouldReturn` expected

    removeStackWorkDir
    specify "other file" $ do
      let src = "./test-file/Definition.hs"
          tgt = "./test-file/Definition2.hs"
          definition2 = testNeovimLsp (Seconds 10) src $ do
              threadDelaySec 1 -- wait for loading
              b <- vim_get_current_buffer'
              waitCallback $ definitionRequest b (9,3) return
          expected = Response . Record
              $  #jsonrpc @= "2.0"
              <: #id @= Just (IDNum 1.0)
              <: #result @= Some (Just
                    [  Record $
                       #uri @= filePathToUri (baseDirectory ++ tail tgt)
                    <: #range @= Record { fields =
                                  #start @= Record (#line @= 4 <: #character @= 0 <: nil)
                               <: #end   @= Record (#line @= 4 <: #character @= 12 <: nil)
                               <: nil }
                    <: nil])
              <: #error @= None <: nil
      definition2 `shouldReturn` expected

  describe "tekito example" $ do
    removeStackWorkDir
    specify "example 1" $ do
      example1 `shouldReturn` ()

-------------------------------------------------------------------------------


testNeovimLsp :: Seconds
              -> FilePath
              -> NeovimLsp a
              -> IO a
testNeovimLsp time file action = do
  initialEnv <- initialEnvM
  testNeovim time initialEnv $
    finally `flip` finalizeLSP $ do
      vim_command' "source ./test-file/init.vim"

      initializeLsp "./" "hie" ["--lsp", "-d", "-l", "/tmp/hie.log"]
      #fileType .== Just "haskell"
      cwd <- filePathToUri <$> errOnInvalidResult (vimCallFunction "getcwd" [])
      pushRequest' @'InitializeK (initializeParam Nothing (Just cwd))

      dispatch [ callbackHandler ]

      vim_command' $ "edit " ++  file
      nvimHsLspOpenBuffer def

      action

example1 :: IO ()
example1 = testNeovimLsp (Seconds 10) "./test-file/hoge.hs" $ do
  b <- vim_get_current_buffer'

  -- didOpen
  ----------
  threadDelaySec 1
  didOpenBuffer b
  -- Redundant doと type errorが帰ってくるはず

  -- didChange
  ------------
  threadDelaySec 1
  nvim_buf_set_lines' b 5 6 False ["  return ()"]
  -- startは含まない, endは含む 上のは6行目を置換している
  -- start=end=6とすると6,7行目の間に挿入される
  --void $ vim_command "update" -- とかしない限り実ファイルに影響はない
  --print' =<< getBufContents b -- 確認用
  didChangeBuffer b
  -- Redundant doのみ帰ってくるはず

  -- Hover
  --------
  threadDelaySec 1
  waitCallback $ hoverRequest b (6,3) (const (return ()))
  -- line,charは0-indexedでvimのと1ずれる(?)
  -- 次のreturnは range (5,2)~(5,8)
  --   6|  return ()
  --   7|123456789
  -- "return :: () -> IO ()"

  -- Exit
  -------
  pushNotification @'ExitK exitParam

-------------------------------------------------------------------------------

threadDelaySec :: MonadIO m => Int -> m ()
threadDelaySec n = liftIO $ threadDelay (n * 1000 * 1000)

print' :: Show a => a -> Neovim env ()
print' x = liftIO $ hPutBuilder stdout (getUtf8Builder (displayShow x))

