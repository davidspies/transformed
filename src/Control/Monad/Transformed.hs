{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Transformed
  ( PassesWriter (..),
    Transformed (..),
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Control
import Data.Kind (Type)
import Prelude

newtype Transformed t (m :: Type -> Type) a = Transformed {runTransformed :: t m a}
  deriving (Functor, Applicative, Monad) via t m
  deriving (MonadTrans, MonadTransControl) via t

class MonadTransControl t => PassesWriter t where
  fOut ::
    Monoid w =>
    (StT t a, w) ->
    StT t (a, w)
  fIn ::
    Monoid w =>
    StT t (a, w -> w) ->
    (StT t a, w -> w)

instance PassesWriter (ReaderT r) where
  fOut = id
  fIn = id

instance Monoid w => PassesWriter (WriterT w) where
  fOut = swapSnds
  fIn = swapSnds

instance PassesWriter (StateT s) where
  fOut = swapSnds
  fIn = swapSnds

instance PassesWriter (ExceptT e) where
  fOut (Left err, _) = Left err
  fOut (Right val, w) = Right (val, w)

  -- If an error is thrown, the written elements are unchanged.
  fIn (Left err) = (Left err, id)
  fIn (Right (val, fn)) = (Right val, fn)

swapSnds :: ((a, b), c) -> ((a, c), b)
swapSnds ((x, y), z) = ((x, z), y)

instance
  (MonadTransControl t, MonadReader r m, Monad (t m)) =>
  MonadReader r (Transformed t m)
  where
  ask = lift ask
  local fn act =
    liftWith (\unlift -> local fn (unlift act)) >>= restoreT . return
  reader fn = lift $ reader fn

instance
  (MonadTransControl t, PassesWriter t, MonadWriter w m, Monad (t m)) =>
  MonadWriter w (Transformed t m)
  where
  writer = lift . writer
  tell = lift . tell
  listen (act :: Transformed t m a) =
    liftWith (\unlift -> listen (unlift act))
      >>= restoreT . return . fOut @t @w @a
  pass (act :: Transformed t m (a, w -> w)) =
    liftWith (\unlift -> pass (fIn @t @w @a <$> unlift act))
      >>= restoreT . return

instance
  (MonadTrans t, MonadState s m, Monad (t m)) =>
  MonadState s (Transformed t m)
  where
  get = lift get
  put = lift . put
  state = lift . state

instance
  (MonadTransControl t, MonadError e m, Monad (t m)) =>
  MonadError e (Transformed t m)
  where
  throwError = lift . throwError

  -- Loses state when an error is caught
  catchError act onError =
    liftWith (\unlift -> unlift act `catchError` \e -> unlift $ onError e)
      >>= restoreT . return
