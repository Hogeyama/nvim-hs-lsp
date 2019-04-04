
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE UndecidableInstances  #-}

module Util where

import           RIO                          hiding ((^.))
import           Control.Monad.IO.Class       (MonadIO)
import           GHC.Stack                    (callStack, popCallStack)

-------------------------------------------------------------------------------
-- Mutable State
-------------------------------------------------------------------------------

useTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> m a
useTV l = readTVarIO =<< view l

usesTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> (a -> b) -> m b
usesTV l f = f <$> useTV l

assignTV :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> a -> m ()
assignTV l x = do
  v <- view l
  atomically $ writeTVar v x

modifyOverTV :: (MonadReader r m, MonadIO m)
             => Lens' r (TVar a) -> (a -> a) -> m ()
modifyOverTV l f = do
  v <- view l
  atomically $ modifyTVar' v f

(.==) :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> a -> m ()
(.==) = assignTV
infix 4 .==

(%==) :: (MonadReader r m, MonadIO m) => Lens' r (TVar a) -> (a -> a) -> m ()
(%==) = modifyOverTV
infix 4 %==

-------------------------------------------------------------------------------
-- Catch and Log
-------------------------------------------------------------------------------

loggingErrorImmortal
  :: (HasCallStack, MonadReader r m, MonadUnliftIO m, HasLogFunc r)
  => m () -> m ()
loggingErrorImmortal = handleAnyDeep $ \e -> logError (displayShow e)
  where ?callstack = popCallStack callStack

loggingError
  :: (HasCallStack, MonadReader r m, MonadUnliftIO m, HasLogFunc r)
  => m a -> m a
loggingError = handleAny $ \e -> logError (displayShow e) >> throwIO e
  where ?callstack = popCallStack callStack

loggingErrorDeep
  :: (HasCallStack, MonadReader r m, MonadUnliftIO m, HasLogFunc r, NFData a)
  => m a -> m a
loggingErrorDeep = handleAnyDeep $ \e -> logError (displayShow e) >> throwIO e
  where ?callstack = popCallStack callStack

