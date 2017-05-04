{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA2)
import Data.Tuple (swap)
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.State

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

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Monad m => Functor (LoggT m) where
  fmap = liftM

instance Monad m => Applicative (LoggT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (LoggT m) where
  return v = LoggT $ return (Logged "" v)
  m >>= k = LoggT $ do
    (Logged str v ) <- runLoggT m
    (Logged str' v') <- runLoggT (k v)
    return $ Logged (str ++ str') v'
  fail = LoggT . fail

logTst :: LoggT Identity Integer
logTst = do
    x <- LoggT $ Identity $ Logged "AAA" 30
    y <- return 10
    z <- LoggT $ Identity $ Logged "BBB" 2
    return $ x + y + z

failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
    5 <- LoggT $ fmap (Logged "") xs
    LoggT [Logged "A" ()]
    return 42

instance (Monoid w) => MonadTrans (WriterT w) where
  lift :: Monad m => m a -> WriterT w m a
  lift m = WriterT $ do
    x <- m
    return (x, mempty)

type Logg = LoggT Identity

write2log :: Monad m => String -> LoggT m ()
write2log = LoggT . return . (flip Logged) ()

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

logTst' :: Logg Integer
logTst' = do
  write2log "AAA"
  write2log "BBB"
  return 42

stLog :: StateT Integer Logg Integer
stLog = do
  modify (+1)
  a <- get
  lift $ write2log $ show $ a * 10
  put 42
  return $ a * 100

instance MonadTrans LoggT where
  lift m = LoggT $ do
    x <- m
    return $ Logged "" x

logSt :: LoggT (State Integer) Integer
logSt = do
    lift $ modify (+1)
    a <- lift get
    write2log $ show $ a * 10
    lift $ put 42
    return $ a * 100
