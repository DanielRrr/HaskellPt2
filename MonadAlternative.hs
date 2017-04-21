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

(<***>) :: Applicative f => f a -> f (a -> b) -> f b
(<***>) = flip (<*>)

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Monad PrsE where
  p >>= f = PrsE fun where
    fun s = do
      (d, s') <- (runPrsE p s)
      runPrsE (f d) s'


data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) z = Bi x y z
concat3OC (Un l) (Bi r1 r2 rt) c = Bi l r1 $ concat3OC (Un r2) rt c
concat3OC (Bi l1 l2 lt) r c = Bi l1 l2 $ concat3OC lt r c
