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
