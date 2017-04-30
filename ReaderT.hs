import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans
import Control.Monad.State

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T $ \x y -> return (f x y)

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T $ \x y z -> return (f x y z)

instance (Functor m) => Functor (Arr2T e1 e2 m) where
  fmap f trans = Arr2T (\x y -> fmap f ((getArr2T trans) x y))

instance (Functor m) => Functor (Arr3T e1 e2 e3 m) where
  fmap f trans = Arr3T (\x y z -> fmap f ((getArr3T trans) x y z))

instance (Applicative m) => Applicative (Arr2T e1 e2 m) where
  pure x = Arr2T $ \y z -> pure x
  f <*> g = Arr2T $ \x y -> (getArr2T f x y) <*> (getArr2T g x y)

instance (Applicative m) => Applicative (Arr3T e1 e2 e3 m) where
  pure x = Arr3T $ \a b c -> pure x
  f <*> g = Arr3T $ \x y z -> (getArr3T f x y z) <*> (getArr3T g x y z)

instance (Monad m) => Monad (Arr2T e1 e2 m) where
  return x = Arr2T $ \y z -> return x
  val >>= f = Arr2T $ \x y -> do
    x1 <- getArr2T val x y
    getArr2T (f x1) x y

instance (Monad m) => Monad (Arr3T e1 e2 e3 m) where
  return x = Arr3T $ \a b c -> return x
  val >>= f = Arr3T $ \a b c -> do
    x1 <- getArr3T val a b c
    getArr3T (f x1) a b c

{-
instance (Monad m) => Monad (Arr3T e1 e2 e3 m) where
  val >>= f = Arr3T $ \a b c -> do
    x1 <- getArr3T val a b c
    getArr3T (f x1) a b c
  fail result = Arr3T $ \a b c -> fail result
-}

instance MonadTrans (Arr2T e1 e2) where
  lift z = Arr2T (\x y -> z)

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \x y -> return (f x y)
