import Text.Read
import Control.Monad.Trans.Except

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead str = case readMaybe str of
  Nothing -> throwE $ NoParse str
  Just result -> return result

data SumError = SumError Int ReadError  deriving Show

trySum :: [String] -> Except SumError Integer
trySum = except . snd . foldl (\ (n, sum) x -> (n + 1, (+) <$> sum <*> tryReadNth n x)) (1, Right 0) where tryReadNth n x = runExcept $ withExcept (SumError n) (tryRead x)
