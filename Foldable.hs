{-# LANGUAGE TypeOperators #-}


import Data.Foldable
import Data.Monoid
import Data.Traversable
import Control.Applicative
{-

class Foldable l where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  fold :: (Monoid m) => t m -> m
  fold = foldr mappend mempty
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f cont = foldr (mappend . f) mempty

instance Foldable [] where
  foldr f ini [] = ini
  foldr f ini (x : xs) = f x (foldr f ini xs)

  foldl f ini [] = ini
  foldl f ini (x : xs) = foldl (f ini x) xs

instance Foldable Maybe where
  foldr f ini Nothing = ini
  foldr f ini (Just x) = f ini x

  foldl f ini Nothing = ini
  foldl f ini (Just x) = f ini x
-}

data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
  foldMap f (Tr x y z) = (f x) `mappend` (f y) `mappend` (f z)

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

instance Foldable Tree where
  foldr f ini Nil = ini
  foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

treeToList :: Tree a -> [a]
treeToList t = foldr (:) [] t


newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Preorder where
  foldr f ini (PreO Nil) = ini
  foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))

instance Functor Preorder where
  fmap f (PreO Nil) = PreO Nil
  fmap f (PreO (Branch l x r)) = PreO (Branch (fmap f l) (f x) (fmap f r))

tree = PreO (Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil)))

test = fst $ sequenceA_ $ (\x -> (show x,x)) <$> tree

instance Foldable Postorder where
  foldr f ini (PostO Nil) = ini
  foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)

instance Foldable Levelorder where
  foldr f ini (LevelO root) = level [root] where
      level [] = ini
      level (Nil : nodes) = level nodes
      level ((Branch l x r) : nodes) = f x $ level (nodes ++ [l, r])

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldr f ini (Cmps x) = foldr (\ a b -> foldr f b a) ini x
