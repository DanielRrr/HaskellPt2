{-# LANGUAGE TypeOperators #-}

infixr 9 |.|

newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show)

type A   = ((,) Integer |.| (,) Char) Bool

type B t = ((,,) Bool (t -> t) |.| Either String) Int

type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (5, ('d', False))

b :: B t
b = Cmps (True, id, Right 666)

c :: C
c  = Cmps (const id)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps x) = Cmps ((fmap (fmap h)) x)

newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) }
  deriving (Eq,Show)

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap f (Cmps3 x) = Cmps3 (fmap (fmap (fmap f)) x)

{-
Докажите выполнение второго закона функторов для композиции двух функторов:

fmap h2 (fmap h1 (Cmps x)) = fmap (h2 . h1) (Cmps x).



1) fmap h2 (fmap h1 (Cmps x)) == fmap h2 (Cmps $ fmap (fmap h1) x) == Cmps $ fmap (fmap h2) (fmap (fmap h1) x)

2) fmap (h2 . h1) (Cmps x) ==
Cmps $ fmap (fmap (h2 . h1) x) ==
Cmps $ fmap ((fmap h2) . (fmap h1)) x --(по закону фукнтора о сохранении композиции)
Cmps $ (fmap (fmap h2) . fmap (fmap  h1)) x --аналогично
Cmps $ fmap (fmap h2) (fmap (fmap h1) x) --раскрытие композиции
-}
