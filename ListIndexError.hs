import Control.Monad.Trans.Except

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)

checkIndex [] index position = True
checkIndex (x : xs) index position | position >= index = False
                                   | otherwise         = checkIndex xs index (position + 1)

infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
arr   !!! index | index < 0    = throwE ErrNegativeIndex
                | checkIndex arr index 0 = throwE $ ErrIndexTooLarge index
                | otherwise              = return $ arr !! index
