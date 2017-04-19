
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
-}
