module Control.Monad.Trans.State.Utils (toStateT) where

import Control.Applicative (Applicative(..))
import Control.Monad.Trans.State (State, StateT, mapStateT)
import Data.Functor.Identity (runIdentity)

toStateT :: Applicative m => State s a -> StateT s m a
toStateT = mapStateT (pure . runIdentity)
