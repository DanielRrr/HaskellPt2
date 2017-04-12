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

{-instance Applicative Parser where
  pure x = Parser f where
    f s = (x, s) -}

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


{- instance Functor PrsE where
  fmap f s = PrsE fun where
    fun s = case f s of
      f [] = Left "exception"
      f (x:xs) = Right (x, xs) -}

{-
instance Applicative PrsE where
--pure :: a -> PrsE a
  pure x = PrsE f where
    f s = Right (x, s)
--(<*>) :: PrsE (a -> b) -> PrsE a -> PrsE b
  (<*>) = undefined -}


{-    instance Alternative Prs where
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
