{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Control.Monad.Trans.Except

add :: Int -> Int -> (Int -> a) -> a
add x y c = c $ x + y

decode f = f 0
as x f = f x
a x f = f x
number = id

one x f = f (x + 1)
two x f = f (x + 2)
three x f = f (x + 3)
seventeen x f = f (x + 17)
twenty x f = f (x + 20)
hundred x f = f (x * 100)
thousand x f = f (x * 1000)

square :: Int -> (Int -> a) -> a
square x f = f (x^2)

sumSquares :: Int -> Int -> (Int -> a) -> a
sumSquares x y f = square x $ \x2 -> square y $ \y2 -> add x2 y2 $ \s -> f s

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

evalCont :: Cont r r -> r
evalCont x = runCont x id

instance Functor (Cont r) where
  fmap = liftM

instance Applicative (Cont r) where
  pure = return
  (<*>) = ap

instance Monad (Cont r) where
  return x = Cont $ \c -> c x
  Cont v >>= k = Cont $ \c -> v (\a -> runCont (k a) c)

{- square' :: Int -> Cont r Int
square' x = return $ x^2

add' :: Int -> Int -> Cont r Int
add' x y = return $ x + y

sumSquares' :: Int -> Int -> Cont r Int
sumSquares' x y = do
  x1 <- square' x
  y1 <- square' y
  result <- add' x1 y1
  return result
-}
showCont :: Show a => Cont String a -> String
showCont x = runCont x show

type Checkpointed a = (a -> Cont [a] a) -> (Cont [a] a)

addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}

safeFilter :: (a -> Bool) -> [a] -> [a]
safeFilter pred lst = case (filter pred lst) of
  [] -> [head lst]
  res -> res

checkpointed :: (a -> Bool) -> Checkpointed a -> Cont r a
checkpointed pred checkpoints = Cont $ \c' ->
    c' $ val where
      val = (head . reverse . (safeFilter pred))(runCont (checkpoints $ \v -> Cont $ \c -> (v : (c v))) (\x -> [x]))

newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

instance Functor (FailCont r e) where
  fmap = liftM

instance Applicative (FailCont r e) where
  pure = return
  (<*>) = ap

instance Monad (FailCont r e) where
  return x = FailCont $ \f _ -> f x
  FailCont val >>= ex = FailCont $ \f x -> val (\y -> runFailCont (ex y) f x) x

toFailCont :: Except e a -> FailCont r e a
toFailCont x = case (runExcept x) of
  Left l -> FailCont (\_ f -> f l)
  Right r -> FailCont (\f _ -> f r)

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont x = runFailCont x Right Left
