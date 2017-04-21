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
