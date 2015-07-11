-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Control.Concurrent.Event (
    -- * Events
    Event
  , newEvent
    -- * Triggering events
  , Trigger(trigger)
  ) where

import Control.Monad ( ap )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Foldable ( traverse_ )
import Data.IntMap as M
import Data.IORef
import Data.Semigroup ( Semigroup(..) )

-- |A @'Event' a@ is a value of type 'a' with no direct representation. It lives
-- /in the future/. It’s possible to register callbacks with 'on' to execute
-- actions when data becomes available, and to detach those actions with the
-- resulting 'Detach' object by calling 'detach' on it.
--
-- 'Event's can be triggered with the 'trigger' function and the associated
-- type 'Trigger'.
newtype Event a = Event { on :: (a -> IO ()) -> IO Detach }

instance Applicative Event where
  pure x = Event $ \k -> k x >> pure mempty
  (<*>) = ap

instance Functor Event where
  fmap f e = Event $ \k -> on e $ k . f

instance Monad Event where
  return = pure
  x >>= f = Event $ \k -> do
    dref <- newIORef mempty
    dx <- on x $ \x' -> do
      dfx <- on (f x') k
      modifyIORef dref (<> dfx)
    modifyIORef dref (<> dx)
    pure . Detach $ readIORef dref >>= detach

instance Monoid (Event a) where
  mempty = Event . const $ pure mempty
  mappend = (<>)

instance Semigroup (Event a) where
  a <> b = Event $ \k -> (<>) <$> on a k <*> on b k

newtype Detach = Detach { detach :: IO () }

instance Monoid Detach where
  mempty = Detach $ pure ()
  mappend = (<>)

instance Semigroup Detach where
  a <> b = Detach $ detach a >> detach b

-- |@'Trigger' a@ is used to 'trigger' an @'Event' a@.
newtype Trigger a = Trigger { trigger :: a -> IO () }

-- |Create a new @'Event' a@ along with a @'Trigger' a@.
newEvent :: (MonadIO m) => m (Event a,Trigger a)
newEvent = liftIO $ do
    callbacksRef <- newIORef M.empty
    hRef <- newIORef 0
    pure (fut callbacksRef hRef,register callbacksRef)
  where
    fut callbacksRef hRef = Event $ \cb -> do
      h <- fmap succ $ readIORef hRef
      modifyIORef callbacksRef $ insert h cb
      writeIORef hRef (succ h)
      pure . Detach . modifyIORef callbacksRef $ delete h
    register ref = Trigger $ \a -> liftIO $ readIORef ref >>= traverse_ ($ a)
