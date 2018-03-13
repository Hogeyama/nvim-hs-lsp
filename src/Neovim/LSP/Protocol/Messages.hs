
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# OPTIONS_GHC -Wall #-}

-- BuildMessageとかのほうがいいかしら
module Neovim.LSP.Protocol.Messages where

import           Data.Extensible hiding (Nullable)
import           Data.Singletons

import           Neovim.LSP.Protocol.Type
import           Neovim.LSP.Protocol.Type.JSON (Option (..))

-------------------------------------------------------------------------------
-- General
-------------------------------------------------------------------------------

notification :: forall (m :: ClientNotificationMethodK). ImplNotification m
             => NotificationParam m -> Notification m
notification a = Notification $ #jsonrpc @= "2.0"
                             <: #method  @= fromSing (sing :: Sing m)
                             <: #params  @= a
                             <: nil

-- | Build 'ClientRequest'
request :: forall (m :: ClientRequestMethodK). ImplRequest m
        => ID
        -> RequestParam m
        -> ClientRequest m
request id' a = Request $ #jsonrpc @= "2.0"
                       <: #id      @= id'
                       <: #method  @= fromSing (sing :: Sing m)
                       <: #params  @= a
                       <: nil

-- | Build 'ClientResponse'
response :: forall (m :: ServerRequestMethodK). SingI m
         => Nullable ID
         -> Option (ResResult m)
         -> Option (ResponseError (ResError m))
         -> ClientResponse m
response id' resp err = Response $ #jsonrpc @= "2.0"
                                <: #id      @= id'
                                <: #result  @= resp
                                <: #error   @= err
                                <: nil

-------------------------------------------------------------------------------
-- Initialize
-------------------------------------------------------------------------------

initializeParam :: Nullable Number -> Nullable Uri -> RequestParam 'InitializeK
initializeParam processId rootUri
   = #processId             @= processId
  <: #rootPath              @= None
  <: #rootUri               @= rootUri
  <: #initializationOptions @= None
  <: #capabilities          @=  ( #workspace    @= None
                               <: #textDocument @= None
                               <: #experimental @= None
                               <: nil )
  <: #trace                 @= None
  <: nil

-------------------------------------------------------------------------------
-- Exit
-------------------------------------------------------------------------------

exitNotification :: Notification 'ExitK
exitNotification = notification exitParam

exitParam :: NotificationParam 'ExitK
exitParam = None

-------------------------------------------------------------------------------
-- DidOpenTextDocument Notification
-------------------------------------------------------------------------------

didOpenTextDocumentParam :: TextDocumentItem
                         -> NotificationParam 'TextDocumentDidOpenK
didOpenTextDocumentParam textDocument = #textDocument @= textDocument <: nil

