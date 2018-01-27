
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE PolyKinds             #-}
{-# OPTIONS_GHC -Wall #-}

-- BuildMessageとかのほうがいいかしら
module Neovim.LSP.Protocol.Messages where

import           Data.Extensible
import           Data.Singletons

import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Protocol.Type.JSON (Option (..))
import qualified Neovim.LSP.Protocol.Type.Key  as K

-------------------------------------------------------------------------------
-- General
-------------------------------------------------------------------------------

-- {-
-- | Build 'ClientNotification'
--
-- Because @m@ is not uniquely determined by type @NotificationParam 'Client m@,
-- type annotation is always required when you use this function.
-- The GHC extension @-XTypeApplication@ is useful to do this. e.g.:
--
-- > notification \@\'Initialize someInitializeParam
--
-- -}
{-notification :: forall (m :: ClientMethod). IsNotification 'Client m
  TODO exitNotificationのNone VoidがJSONを実装してないのでダメ
-}
notification :: forall (m :: ClientMethodK). ImplNotification m
             => NotificationParam m -> Notification m
notification a = Notification $ K.jsonrpc @= "2.0"
                             <: K.method  @= fromSing (sing :: Sing m)
                             <: K.params  @= a
                             <: nil

-- | Build 'ClientRequest'
request :: forall (m :: ClientMethodK). ImplRequest m
        => ID
        -> RequestParam m
        -> ClientRequest m
request id' a = Request $ K.jsonrpc @= "2.0"
                       <: K.id      @= id'
                       <: K.method  @= fromSing (sing :: Sing m)
                       <: K.params  @= a
                       <: nil

-- | Build 'ClientResponse'
response :: forall (m :: ServerMethodK). SingI m
         => Maybe ID
         -> ResResult m
         -> Option (ResponseError (ResError m))
         -> ClientResponse m
response id' res err = Response $ K.jsonrpc @= "2.0"
                               <: K.id      @= id'
                               <: K.result  @= res
                               <: K.error   @= err
                               <: nil

-------------------------------------------------------------------------------
-- Initialize
-------------------------------------------------------------------------------

initializeParam :: Maybe Number -> Maybe Uri -> RequestParam 'InitializeK
initializeParam processId rootUri
   = K.processId             @= processId
  <: K.rootPath              @= None
  <: K.rootUri               @= rootUri
  <: K.initializationOptions @= None
  <: K.capabilities          @=  ( K.workspace    @= None
                                <: K.textDocument @= None
                                <: K.experimental @= None
                                <: nil )
  <: K.trace                 @= None
  <: nil

-------------------------------------------------------------------------------
-- Exit
-------------------------------------------------------------------------------

exitNotification :: ClientNotification 'ExitK
exitNotification = notification exitParam

exitParam :: NotificationParam 'ExitK
exitParam = None

-------------------------------------------------------------------------------
-- DidOpenTextDocument Notification
-------------------------------------------------------------------------------

didOpenTextDocumentParam :: TextDocumentItem
                         -> NotificationParam 'TextDocumentDidOpenK
didOpenTextDocumentParam textDocument = K.textDocument @= textDocument <: nil

