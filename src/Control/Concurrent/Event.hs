-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- An @'Event' a@ is an object representing an event of type 'a'. You can
-- register actions through it – see the 'on' function – and detach them later
-- on.
--
-- An 'Event' has many purposes. The one in mind when writing that package was
-- to interface over __C__ callback-based reactive system. Consider the
-- following __Haskell__ wrapper function, which is based on imperative style:
--
-- @
--   -- Create a new button and register an action to launch when the button’s
--   -- state changes.
--   createButton :: (ButtonState -> IO ()) -> IO Button
--   createButton callback = do
--     -- create the button
--     button <- ...
--     forkIO . forever $ do
--       -- launch a thread in which we can test whether the state has changed
--       when stateHasChanged $ callback newState
--     pure button
-- @
--
-- We can enhance that by representing the action of registering to the event
-- and detaching from it by immediately returning a value:
--
-- @
--   createButton :: IO (Button,Event ButtonState)
--   createButton = do
--     -- create the button
--     button <- ...
--     -- create an 'Event'
--     (ev,t) <- newEvent
--     forkIO . forever $
--       -- check the new state
--       when stateHasChanged $ trigger t newState
--     pure (button,ev)
-- @
--
-- The 'Trigger' can also be returned to manually invoke the 'Event'.
----------------------------------------------------------------------------

module Control.Concurrent.Event (
    -- * Events
    Event
  , Detach(..)
  , on
  , newEvent
    -- * Triggering events
  , Trigger(..)
  , trigger
    -- * Event combinators
  , filterE
  , foldrE
  ) where

import Control.Monad ( ap, when )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Foldable ( traverse_ )
import Data.IntMap as M
import Data.IORef
import Data.Semigroup ( Semigroup(..) )

-- |An @'Event' a@ is a value of type 'a' with no direct representation. It lives
-- /in the future/. It’s possible to register actions with 'on' to execute
-- when data becomes available, and to detach those actions with the
-- resulting 'Detach' object by calling 'detach' on it.
--
-- 'Event's can be triggered with the 'trigger' function and the associated
-- type 'Trigger'.
newtype Event a = Event { unEvent :: (a -> IO ()) -> IO Detach }

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

-- |Register an action.
on :: (MonadIO m) => Event a -> (a -> IO ()) -> m Detach
on (Event register) f = liftIO $ register f

-- |'Detach' is used to detach an action from an 'Event'.
newtype Detach = Detach { detach :: IO () }

instance Monoid Detach where
  mempty = Detach $ pure ()
  mappend = (<>)

instance Semigroup Detach where
  a <> b = Detach $ detach a >> detach b

-- |@'Trigger' a@ is used to 'trigger' an @'Event' a@.
newtype Trigger a = Trigger (a -> IO ())

instance Monoid (Trigger a) where
  mempty = Trigger . const $ pure ()
  mappend = (<>)
  
instance Semigroup (Trigger a) where
  Trigger f <> Trigger g = Trigger $ \a -> f a >> g a

-- |Use a 'Trigger'.
trigger :: (MonadIO m) => Trigger a -> a -> m ()
trigger (Trigger f) = liftIO . f

-- |Create a new @'Event' a@ along with a @'Trigger' a@.
newEvent :: (MonadIO m) => m (Event a,Trigger a)
newEvent = liftIO $ do
    callbacksRef <- newIORef M.empty
    hRef <- newIORef 0
    pure (fut callbacksRef hRef,register callbacksRef)
  where
    fut callbacksRef hRef = Event $ \cb -> do
      h <- succ <$> readIORef hRef
      modifyIORef callbacksRef $ insert h cb
      writeIORef hRef (succ h)
      pure . Detach . modifyIORef callbacksRef $ delete h
    register ref = Trigger $ \a -> liftIO $ readIORef ref >>= traverse_ ($ a)

-- |Filter an 'Event' with a predicate.
filterE :: (a -> Bool) -> Event a -> Event a
filterE predicate e = Event $ \k -> do
  (filtered,trig) <- newEvent
  _ <- on e $ \a -> when (predicate a) (trigger trig a)
  unEvent filtered k

-- |Right fold an 'Event'. Each time an event occur, the function folding function is applied and
-- the result is passed to the future 'Event'.
foldrE :: (b -> a -> b) -> b -> Event a -> Event b
foldrE f b e = Event $ \k -> do
  (folded,trig) <- newEvent
  ref <- newIORef b
  _ <- on e $ \a -> do
    acc <- readIORef ref
    let x = f acc a
    writeIORef ref x
    trigger trig x
  unEvent folded k

