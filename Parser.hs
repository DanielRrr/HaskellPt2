import Data.Char
import Control.Applicative hiding (many)

newtype Parser a = Parser {apply :: String -> [(a, String)]}

parse :: Parser a -> String -> a
parse p = fst . head . apply p

anyChar :: Parser Char
anyChar = Parser f where
  f "" = []
  f (x:xs) = [(x,xs)]

instance Functor Parser where
  fmap f p = Parser fun where
    fun s = [(f a, s') | (a, s') <- apply p s]

instance Applicative Parser where
  pure a = Parser f where
    f s = [(a, s)]
  pf <*> pv = Parser f where
    f s = [(g a, s'') | (g, s') <- apply pf s, (a, s'') <- apply pv s']

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser f where
  f "" = []
  f (x:xs) | pr x = [(x, xs)]
           | otherwise = []

lower :: Parser Char
lower = satisfy isLower

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

char :: Char -> Parser Char
char c = satisfy (== c)

multiplication :: Parser Int
multiplication = (*) <$> digit <* char '*' <*> digit

sum :: Parser Int
sum = (+) <$> digit <* char '+' <*> digit

instance Alternative Parser where
  empty = Parser f where
    f = const []
  p1 <|> p2 = Parser f where
    f s = (apply p1 s) <|> (apply p2 s)

lowers :: Parser String
lowers = pure (:) <*> lower <*> lowers <|> pure ""

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f (Prs g) = Prs fun where
    fun s = case g s of
      Nothing -> Nothing
      Just (x, y) -> Just (f x, y)

anyChr :: Prs Char
anyChr = Prs fun where
  fun [] = Nothing
  fun (x:xs) = Just (x, xs)

instance Applicative Prs where
  pure x = Prs f where
    f s = Just (x, s)
  (Prs p1) <*> (Prs p2) = Prs fun where
    fun s = case p1 s of
      Nothing -> Nothing
      Just (f, s') -> case p2 s' of
        Nothing -> Nothing
        Just (x, s'') -> Just (f x, s'')


newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE property = PrsE parser where
  parser "" = Left "exception: empty string"
  parser (x:xs) = case property x of
    False -> Left ("exception " ++ ['x'])
    True -> Right (x, xs)

charE :: Char -> PrsE Char
charE c = satisfyE (== c)


instance Functor PrsE where
    -- fmap :: (a -> b) -> PrsE a -> PrsE b
    fmap f (PrsE g) = PrsE fun where
        fun s = case g s of
            Left e -> Left e
            Right (x, s') -> Right (f x, s')


instance Applicative PrsE where
  pure x = PrsE f where
    f s = Right (x, s)
  (PrsE p1) <*> (PrsE p2) = PrsE f where
    f s = case p1 s of
      Left x -> Left x
      Right (f, string) -> case p2 string of
        Left x -> Left x
        Right (x, string') -> Right (f x, string')


instance Alternative Prs where
  empty = Prs f where
    f = const Nothing
  p1 <|> p2 = Prs f where
    f s = (runPrs p1 s) <|> (runPrs p2 s)

many1 :: Prs a -> Prs [a]
many1 = undefined

nat :: Prs Int
nat = undefined
