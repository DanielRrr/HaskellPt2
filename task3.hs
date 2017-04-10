import Text.Parsec
import Control.Applicative ((*>), (<*))

getList :: Parsec String u [String]
getList = ((many1 digit) `sepBy` (char ';'))

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces x y z = x *> z <* y

a = parseTest digit "12AB"

b = parseTest letter "AB12AB"

c = parse letter "" "AB12AB"

vowel :: Parsec [Char] u Char
vowel = oneOf "aeiou"

vowel1 :: Parsec [Char] u Char
vowel1 = oneOf "dbcvgs"


{- newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

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

instance Alternative PrsE where
  empty = PrsE f where
    f _ = Left "empty alternative"
  p <|> q = PrsE f where
    f s = let ps = runPrsE p s
      in if null ps
         then runPrsE q s
         else ps -}
