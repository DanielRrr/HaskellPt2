module Demo where

import Control.Monad (liftM, ap, MonadPlus(mzero, mplus), guard, msum)
import Control.Applicative (Alternative(empty, (<|>)))

newtype Except e a = Except { runExcept :: Either e a} deriving Show

except :: Either e a -> Except e a
except = Except

instance Functor (Except e) where
  fmap f (Except x) = Except (fmap f x)

instance Applicative (Except e) where
  pure x = Except (Right x)
  (Except f) <*> (Except x) = Except (f <*> x)


instance Monad (Except e) where
  return a = Except (Right a)
  m >>= k =
    case runExcept m of
      (Left e) -> Except (Left e)
      (Right x) -> k x

throwE :: e -> Except e a
throwE = except . Left

catchE :: Except e a -> (e -> Except e' a) -> Except e' a
m `catchE` h =
  case runExcept m of
    Left e -> h e
    Right x -> except (Right x)

data DivByError = ErrZero String | ErrOther deriving (Eq, Show)

(/?) :: Double -> Double -> Except DivByError Double
x /? 0 = throwE $ ErrZero (show x ++ "/0;")
x /? y = return $ (x / y)

example :: Double -> Double -> Except DivByError String
example x y = action `catchE` handler where
  action = do
    q <- x /? y
    return $ show q
  handler = \err -> return $ show err

instance Monoid e => Alternative (Except e) where
  empty = mzero
  (<|>) = mplus

instance Monoid e => MonadPlus (Except e) where
  mzero = Except (Left mempty)
  Except x `mplus` Except y = Except $
    case x of
      Left e -> either (Left . mappend e) Right y
      r -> r

instance Monoid DivByError where
  mempty = ErrOther
  ErrZero s1 `mappend` ErrZero s2 = ErrZero $ s1 ++ s2
  ErrOther `mappend` ErrOther = ErrOther
  x `mappend` ErrOther = x
  ErrOther `mappend` x = x

example1 :: Double -> Double -> Except DivByError String
example1 x y = action `catchE` handler where
  action = do
    q <- x /? y
    guard $ y >= 0
    return $ show q
  handler (ErrZero s) = return s
  handler ErrOther = return "NONNEGATIVE GUARD"
