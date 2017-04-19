{-# LANGUAGE TypeOperators #-}

import Data.Foldable
import Data.Traversable
import Control.Applicative

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldr f ini Nil = ini
  foldr f ini (Branch l x r) = f x (foldr f (foldr f ini r) l)

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

{-
sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())

foldMap :: Monoid m => (a -> m) -> t a -> m
foldMap f cont = foldr (mappend . f) mempty

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ = foldr ((*>) . f) (pure ())
-}

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (\x y -> (:) <$> f x <*> y) (pure [])

sequenceA2list :: (Foldable t, Applicative f) => t (f a) -> f [a]
sequenceA2list = foldr (\x y -> (:) <$> x <*> y) (pure [])

{-
class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse g (Just x) = pure Just <*> g x

instance Traversable ((,) s) where
  traverse :: Applicative f => (a -> f b) -> (s, a) -> f (s, b)
  traverse f (x, y) = (pure (,) x) <*> f y
-}

data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
  fmap g (Tr x y z) = Tr (g x) (g y) (g z)

instance Applicative Triple where
  pure x = Tr x x x
  (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)

instance Foldable Triple where
  foldMap f (Tr x y z) = (f x) `mappend` (f y) `mappend` (f z)

instance Traversable Triple where
  traverse f (Tr x y z) = Tr <$> f x <*> f y <*> f z

data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
  fmap f (Ok x) = Ok (f x)
  fmap f (Error s) = Error s

instance Applicative Result where
  pure x = Ok x
  Error s <*> _ = Error s
  _ <*> Error s = Error s
  (Ok f) <*> (Ok x) = Ok (f x)

instance Foldable Result where
  foldr f ini (Ok x) = f x ini
  foldr _ ini (Error s) = ini

instance Traversable Result where
  traverse f (Ok x) = Ok <$> f x
  traverse f (Error s) = pure (Error s)

{-
instance Traversable List where
  traverse _ [] = pure []
  traverse g (x:xs) = (:) <$> g x <*> traverse g xs
-}

instance Traversable Tree where
  traverse _ Nil = pure Nil
  traverse f (Branch l x r) = Branch <$> (traverse f l) <*> f x <*> (traverse f r)

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps x) = Cmps ((fmap (fmap h)) x)

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldr f ini (Cmps x) = foldr (\ y acc -> foldr f acc y) ini x

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
  traverse f (Cmps x) = Cmps <$> (traverse . traverse) f x
