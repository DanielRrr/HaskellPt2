{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Bifunctor
import Control.Monad (liftM, ap, MonadPlus(mzero, mplus), guard, msum)
import Control.Applicative (Alternative(empty, (<|>)))

newtype Except e a = Except { runExcept :: Either e a} deriving (Show, Functor, Bifunctor, Applicative, Monad)

except :: Either e a -> Except e a
except = Except

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept = first
