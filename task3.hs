import Text.Parsec

{-getList :: Parsec String u [String]
getList = undefined

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces = undefined -}

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap = undefined

anyChr :: Prs Char
anyChr = undefined

instance Applicative Prs where
  pure  = undefined
  (<*>) = undefined

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE = undefined

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

instance Functor PrsE where
  fmap = undefined

instance Applicative PrsE where
  pure  = undefined
  (<*>) = undefined

instance Alternative Prs where
  empty = undefined
  (<|>) = undefined

many1 :: Prs a -> Prs [a]
many1 = undefined

nat :: Prs Int
nat = undefined
