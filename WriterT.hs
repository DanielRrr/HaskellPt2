{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA2)
import Data.Tuple (swap)

newtype Writer w a = Writer { runWriter :: (a, w)}

newtype WriterT w m a = WriterT { runWriterT :: m (a, w)}

writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT  = fmap snd . runWriterT

instance Functor (Writer w) where
  fmap f = Writer . updater . runWriter
    where updater ~(x, log) = (f x, log)

instance Functor m => Functor (WriterT w m) where
  fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
  fmap f = WriterT . fmap updater . runWriterT
    where updater ~(x, log) = (f x, log)

instance Monoid w => Applicative (Writer w) where
  pure :: a -> Writer w a
  pure x = Writer (x, mempty)
  (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
  f <*> v = Writer $ updater (runWriter f) (runWriter v)
    where updater ~(g, v) ~(x, w) = (g x, v `mappend` w)

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure :: a -> WriterT w m a
  pure x = WriterT $ pure (x, mempty)
  (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
  f <*> v = WriterT $ liftA2 updater (runWriterT f) (runWriterT v)
    where updater ~(g, v) ~(x, w) = (g x, v `mappend` w)

newtype StrictWriter w a = StrictWriter { runStrictWriter :: (a, w) }

instance Functor (StrictWriter w) where
  fmap f  = StrictWriter . updater . runStrictWriter
    where updater (x, log) = (f x, log)

instance Monoid w => Applicative (StrictWriter w) where
  pure x  = StrictWriter (x, mempty)
  f <*> v = StrictWriter $ updater (runStrictWriter f) (runStrictWriter v)
    where updater (g, w) (x, w') = (g x, w `mappend` w')

instance Monoid w => Monad (Writer w) where
  (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  m >>= k = Writer $ let
    (v, w) = runWriter m
    (v', w') = runWriter (k v)
    in (v', w `mappend` w')

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return :: a -> WriterT w m a
  return x = writer (x, mempty)
  (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
  m >>= k  = WriterT $ do
    ~(v, w)   <- runWriterT m
    ~(v', w') <- runWriterT (k v)
    return (v', w `mappend` w')
  fail :: String -> WriterT w m a
  fail = WriterT . fail
