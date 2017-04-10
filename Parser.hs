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

    {- instance Applicative Prs where
      -- pure :: a -> Prs a
      pure x = Prs x
      -- (<*>) :: Prs (a -> b) -> Prs a -> Prs b
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
