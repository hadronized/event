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

module Control.Concurrent.Future (
    -- * Events
    Event
  , newEvent
    -- * Fulfilling events
  , Fulfil(fulfil)
  ) where

import Control.Monad ( ap )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Foldable ( traverse_ )
import Data.IntMap as M
import Data.IORef
import Data.Semigroup ( Semigroup(..) )

-- |A @Event a@ is a value of type 'a' with no direct representation. It lives
-- /in the future/. It’s possible to register callbacks with 'on' to execute
-- actions when data becomes available, and to detach those actions with the
-- resulting 'Detach' object by calling 'detach' on it.
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

-- |@Fulfil a@ is used to 'fulfil' an @Event a@.
newtype Fulfil a = Fulfil { fulfil :: a -> IO () }

-- |Create a new @Event a@ along with a @Fulfil a@.
newEvent :: (MonadIO m) => m (Event a,Fulfil a)
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
    register ref = Fulfil $ \a -> liftIO $ readIORef ref >>= traverse_ ($ a)
