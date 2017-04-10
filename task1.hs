module Task1 where

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }

newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

compose2 :: (t3 -> t2) -> (t1 -> t -> t3) -> t1 -> t -> t2
compose2 g f x y = g (f x y)

compose3 :: (t4 -> t3) -> (t2 -> t1 -> t -> t4) -> t2 -> t1 -> t -> t3
compose3 g f x y z = g (f x y z)

--fmap for Arr2 has type (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 g) = Arr2 (compose2 f g)

-- fmap for Arr3 has type (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 g) = Arr3 (compose3 f g)

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 (\a b -> x)
  (Arr2 f) <*> (Arr2 g) =  Arr2 (\a b -> f a b (g a b))

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 (\a b c -> x)
  (Arr3 f) <*> (Arr3 g) = Arr3 (\a b c -> f a b c (g a b c))

data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
  pure x = Tr x x x
  (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)

newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) }
    deriving (Eq,Show)

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap f (Cmps3 x) = Cmps3 (fmap (fmap (fmap f)) x)

{- unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = undefined

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = undefined -}


{- type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer -}
