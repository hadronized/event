module Control.Concurrent.Future (
    -- * Future values
    Future
  , newFuture
    -- * Working with 'Future'
  , asSoonAs
  , nestWith
  ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Foldable ( traverse_ )
import Data.IntMap as M
import Data.IORef

-- |A @Future a@ is a value of type 'a' with no direct representation. It lives
-- /in the future/.
newtype Future a = Future ( (a -> IO ()) -> IO (IO ()) )

-- |Create a new @Future a@ along with a /trigger/, @a -> IO ()@. The /trigger/
-- should be invoked
newFuture :: (MonadIO m) => m (Future a,a -> IO ())
newFuture = liftIO $ do
    callbacksRef <- newIORef M.empty
    hRef <- newIORef 0
    pure (fut callbacksRef hRef,register callbacksRef)
  where
    fut callbacksRef hRef = Future $ \cb -> do
      h <- fmap succ $ readIORef hRef
      modifyIORef callbacksRef $ insert h cb
      writeIORef hRef (succ h)
      pure . modifyIORef callbacksRef $ delete h
    register ref a = liftIO $ readIORef ref >>= traverse_ ($ a)

-- |@f `asSoonAs` fut@ starts producing with 'f' as soon as 'fut' occurs. That
-- function returns a /clean-up action/, @m ()@, that you can use to cancel your
-- 'f' producer.
asSoonAs :: (MonadIO m) => (a -> IO ()) -> Future a -> m (m ())
asSoonAs f (Future register) = liftIO . fmap liftIO $ register f

-- |From a @Future a@ we can create a @Future b@ that is bound to the former
-- one if we provide a function 'f' @a -> b@. When the parent @Future a@ occurs,
-- the child @Future b@ occurs as well, applying the provided function. The
-- later is **nested** inside the former with a function.
--
-- That function returns a /clean-up action/ you can use to detach the nested
-- @Future b@. After that, it won’t ever produce again.
nestWith :: (Applicative m,MonadIO m)
        => Future a
        -> (a -> b)
        -> m (Future b,m ())
nestWith futA f = do
  (futB,triggerB) <- newFuture
  release <- (triggerB . f) `asSoonAs` futA
  pure (futB,release)
