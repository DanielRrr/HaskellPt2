{-
($)     ::                      (a -> b) ->   a ->   b
(<$>)   :: Functor     f  =>    (a -> b) -> f a -> f b
(<*>)   :: Applicative f  =>  f (a -> b) -> f a -> f b
(=<<)   :: Monad       m  =>  (a -> m b) -> m a -> m b

(&)     ::                      a ->   (a -> b) ->   b  -- Data.Function
(&) = flip ($)

(<&>)   :: Functor     f  =>  f a ->   (a -> b) -> f b  -- Control.Lens.Operators
(<&>) = flip (<$>)

(<**>)  :: Applicative f  =>  f a -> f (a -> b) -> f b  -- Control.Applicative
xs (<**>) fs = (pure &) <*> xs <*> fs

(>>=)   :: Monad       m  =>  m a -> (a -> m b) -> m b
-}

import Data.Monoid

(<***>) :: Applicative f => f a -> f (a -> b) -> f b
(<***>) = flip (<*>)


data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) z = Bi x y z
concat3OC (Un l) (Bi r1 r2 rt) c = Bi l r1 $ concat3OC (Un r2) rt c
concat3OC (Bi l1 l2 lt) r c = Bi l1 l2 $ concat3OC lt r c

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x)        =       x
concatOC (Bi a b rest) = concat3OC a b (concatOC rest)

instance Functor OddC where
  fmap f (Un a) = Un (f a)
  fmap f (Bi a b rest) = Bi (f a) (f b) (fmap f rest)

instance Applicative OddC where
  pure a = (Un a)
  (Un fa) <*> (Un a) = Un (fa a)
  (Bi fa fb frest) <*> (Un a) = (Bi (fa a) (fb a) (frest <*> (Un a)))
  (Un fa) <*> (Bi a b rest) = (Bi (fa a) (fa b) (Un fa <*> rest))
  (Bi fa fb frest) <*> b =
    let
      a' = (Un fa) <*> b
      b' = (Un fb) <*> b
      rest' = frest <*> b
    in
      concat3OC a' b' rest'

instance Monad OddC where
  oddc >>= f = concatOC (fmap f oddc)

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mzero = empty
  mplus :: m a -> m a -> m a
  mplus = (<|>)

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> r = r
  r <|> _ = r

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance MonadPlus [] where


instance MonadPlus Maybe where
  mzero = empty
  mplus = (<|>)
