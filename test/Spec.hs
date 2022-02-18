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
  deriving (Functor, Applicative, Monad, MonadTrans, MonadTransControl, PassesWriter)
  deriving
    (MonadReader r', MonadWriter w', MonadState s', MonadError e')
    via Transformed (MyReaderT r) m

newtype MyWriterT w m a = MyWriterT (WriterT w m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadTransControl, PassesWriter)
  deriving
    (MonadReader r', MonadWriter w', MonadState s', MonadError e')
    via Transformed (MyWriterT w) m

newtype MyStateT s m a = MyStateT (StateT s m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadTransControl, PassesWriter)
  deriving
    (MonadReader r', MonadWriter w', MonadState s', MonadError e')
    via Transformed (MyStateT s) m

newtype MyExceptT e m a = MyExceptT (ExceptT e m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadTransControl, PassesWriter)
  deriving
    (MonadReader r', MonadWriter w', MonadState s', MonadError e')
    via Transformed (MyExceptT e) m
