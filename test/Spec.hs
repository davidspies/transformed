{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Control (MonadTransControl)
import Control.Monad.Transformed (PassesWriter, Transformed (..))
import Control.Monad.Writer (MonadWriter, WriterT)
import Prelude

main :: IO ()
main = putStrLn "Runtime test suite not yet implemented"

newtype MyReaderT r m a = MyReaderT (ReaderT r m a)
  deriving (Functor, Applicative, Monad) via (ReaderT r m)
  deriving (MonadTrans, MonadTransControl, PassesWriter) via (ReaderT r)

deriving via
  Transformed (MyReaderT r) m
  instance
    (MonadReader r') m => MonadReader r' (MyReaderT r m)

deriving via
  Transformed (MyReaderT r) m
  instance
    (MonadWriter w') m => MonadWriter w' (MyReaderT r m)

deriving via
  Transformed (MyReaderT r) m
  instance
    (MonadError e') m => MonadError e' (MyReaderT r m)

deriving via
  Transformed (MyReaderT r) m
  instance
    (MonadState s') m => MonadState s' (MyReaderT r m)

newtype MyWriterT w m a = MyWriterT (WriterT w m a)
  deriving (Functor, Applicative, Monad) via (WriterT w m)
  deriving (MonadTrans, MonadTransControl, PassesWriter) via (WriterT w)

deriving via
  Transformed (MyWriterT w) m
  instance
    (Monoid w, MonadReader r' m) => MonadReader r' (MyWriterT w m)

deriving via
  Transformed (MyWriterT w) m
  instance
    (Monoid w, MonadWriter w' m) => MonadWriter w' (MyWriterT w m)

deriving via
  Transformed (MyWriterT w) m
  instance
    (Monoid w, MonadError e' m) => MonadError e' (MyWriterT w m)

deriving via
  Transformed (MyWriterT w) m
  instance
    (Monoid w, MonadState s' m) => MonadState s' (MyWriterT w m)

newtype MyStateT s m a = MyStateT (StateT s m a)
  deriving (Functor, Applicative, Monad) via (StateT s m)
  deriving (MonadTrans, MonadTransControl, PassesWriter) via (StateT s)

deriving via
  Transformed (MyStateT s) m
  instance
    (MonadReader r') m => MonadReader r' (MyStateT s m)

deriving via
  Transformed (MyStateT s) m
  instance
    (MonadWriter w') m => MonadWriter w' (MyStateT s m)

deriving via
  Transformed (MyStateT s) m
  instance
    (MonadError e') m => MonadError e' (MyStateT s m)

deriving via
  Transformed (MyStateT s) m
  instance
    (MonadState s') m => MonadState s' (MyStateT s m)

newtype MyExceptT e m a = MyExceptT (ExceptT e m a)
  deriving (Functor, Applicative, Monad) via (ExceptT e m)
  deriving (MonadTrans, MonadTransControl, PassesWriter) via (ExceptT e)

deriving via
  Transformed (MyExceptT e) m
  instance
    (MonadReader r') m => MonadReader r' (MyExceptT e m)

deriving via
  Transformed (MyExceptT e) m
  instance
    (MonadWriter w') m => MonadWriter w' (MyExceptT e m)

deriving via
  Transformed (MyExceptT e) m
  instance
    (MonadError e') m => MonadError e' (MyExceptT e m)

deriving via
  Transformed (MyExceptT e) m
  instance
    (MonadState s') m => MonadState s' (MyExceptT e m)
