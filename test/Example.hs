{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import Control.Monad.Except (ExceptT (..), MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Control (MonadTransControl (..))
import Control.Monad.Transformed (PassesWriter (..), Transformed (..))
import Control.Monad.Writer.Lazy (MonadWriter)
import Data.Bifunctor (first)

newtype MyTransformer m a = MyTransformer {getMyTransformer :: ExceptT Int (StateT Bool m) a}
  deriving newtype (Functor, Applicative, Monad)
  deriving
    (MonadReader r, MonadWriter w, MonadState s, MonadError e)
    via Transformed MyTransformer m

instance MonadTrans MyTransformer where
  lift = MyTransformer . lift . lift

instance MonadTransControl MyTransformer where
  type StT MyTransformer a = StT (StateT Bool) (StT (ExceptT Int) a)
  liftWith f =
    MyTransformer $ liftWith $ \run1 -> liftWith $ \run2 -> f (run2 . run1 . getMyTransformer)
  restoreT act = MyTransformer $ restoreT $ restoreT act

instance PassesWriter MyTransformer where
  fOut = first (fOut @(ExceptT Int)) . fOut @(StateT Bool)
  fIn = fIn @(StateT Bool) . first (fIn @(ExceptT Int))
