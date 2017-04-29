{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)
import Data.Char (toUpper)

secondElem :: Reader [String] String
secondElem = do
  el2 <- asks (map toUpper . head . tail)
  return el2

logFirst :: [String] -> Writer String String
logFirst xs = do
  let el1 = head xs
  let el2 = (map toUpper . head . tail) xs
  tell el1
  return el2

{-  logFirstAndSecond :: ReaderT [String] (Writer String) String
logFirstAndSecond = do
  el1 <- asks head
  el2 <- asks (map toUpper . head . tail)
  lift $ (tell el1)
  return el2

logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
      el1 <- lift (asks head)
      el2 <- lift (asks (map toUpper . head . tail))
      tell el1
      return el2
-}

{- pierceArrow :: Bool -> Bool -> Bool
pierceArrow x y = not (x || y)

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate pred1 pred2 xs = do
    let filtered1 = filter pred1 xs
    let filtered2 = filter pred2 xs
    let not_fish_not_meat = filter (\x -> pierceArrow (pred1 x) (pred2 x)) xs
    lift $ tell filtered2
    tell filtered1
    return not_fish_not_meat  -}
