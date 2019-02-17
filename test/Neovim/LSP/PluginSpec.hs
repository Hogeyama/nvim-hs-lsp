
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Neovim.LSP.PluginSpec (spec) where

import           Prelude
import           RIO
import           RIO.Partial                       (fromJust)
import           RIO.List.Partial                  (tail)
import           Test.Hspec
import           Neovim.Test.Wrapper
import           Neovim.Context.Internal
import           Path

import           Neovim

import           LSP
import           Neovim.LSP.Action.Notification
import           Neovim.LSP.Action.Request
import           Neovim.LSP.LspPlugin.Callback
import           Neovim.LSP.Base
import           Neovim.LSP.Plugin
import           Neovim.LSP.Util
import           System.Directory                  (getCurrentDirectory, removeDirectoryRecursive)

spec :: Spec
spec = do
  Just baseDirectory <- parseAbsDir <$> runIO getCurrentDirectory
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
      let src = $(mkRelFile "./test-file/Definition.hs")
          definition1 = testWithHie (Seconds 10) src $ do
              threadDelaySec 1 -- wait for loading
              b <- vim_get_current_buffer'
              waitCallback $ definitionRequest b (8,11) return
          expected = Response . Record
              $  #jsonrpc @= "2.0"
              <: #id @= Just (IDNum 2.0)
              <: #result @= Some (Just
                    [ Record $
                       #uri @= pathToUri (baseDirectory </> src)
                    <: #range @= Record { fields =
                                  #start @= Record (#line @= 11 <: #character @= 0 <: nil)
                               <: #end   @= Record (#line @= 11 <: #character @= 4 <: nil)
                               <: nil }
                    <: nil])
              <: #error @= None <: nil
      definition1 `shouldReturn` expected

    removeStackWorkDir
    specify "other file" $ do
      let src = $(mkRelFile "test-file/Definition.hs")
          tgt = $(mkRelFile "test-file/Definition2.hs")
          definition2 = testWithHie (Seconds 10) src $ do
              threadDelaySec 1 -- wait for loading
              b <- vim_get_current_buffer'
              waitCallback $ definitionRequest b (9,3) return
          expected = Response . Record
              $  #jsonrpc @= "2.0"
              <: #id @= Just (IDNum 2.0)
              <: #result @= Some (Just
                    [  Record $
                       #uri @= pathToUri (baseDirectory </> tgt)
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


testWithHie
  :: Show a
  => Seconds
  -> Path Rel File 
  -> Neovim LanguageEnv a
  -> IO a
testWithHie time file action = do
  initialEnv <- initialEnvM
  testNeovim time initialEnv $ do
    finally `flip` finalizeLSP $ do
      vim_command' "source ./test-file/init.vim"
      cwd <- getCwd
      startServer "haskell" cwd "hie-wrapper" ["--lsp", "-d", "-l", "/tmp/hie.log"]
        [ callbackHandler ]
      void $ focusLang "haskell" $
        sendRequest' @'InitializeK (initializeParam Nothing (Just (pathToUri cwd)))
      vim_command' $ "edit " ++ toFilePath (cwd </> file)
      nvimHsLspOpenBuffer def
      x <- fromJust <$> focusLang "haskell" action
      -- TODO テストだとなぜかtimeoutが効かないみたい？
      -- stopServer内のtimeoutの時間を短くすると再現
      stopServer "haskell"
      return x

example1 :: IO ()
example1 = testWithHie (Seconds 10) $(mkRelFile "./test-file/hoge.hs") $ do
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
  sendNotification @'ExitK exitParam

-------------------------------------------------------------------------------

threadDelaySec :: MonadIO m => Int -> m ()
threadDelaySec n = liftIO $ threadDelay (n * 1000 * 1000)

