import Text.Parsec
import Control.Applicative ((*>), (<*))

getList :: Parsec String u [String]
getList = ((many1 digit) `sepBy` (char ';'))

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces x y z = x *> z <* y

a = parseTest digit "12AB"

b = parseTest letter "AB12AB"

c = parse letter "" "AB12AB"

d = parseTest (many1 digit) "1234567890asdfghjkl"

e = parseTest (many1 letter) "qwertyuiop1234567890asdfghjkl"

f = parseTest (many digit) "1244wrgwr"

g = parseTest (count 3 digit) "35135ianievni"

h = parseTest (count 3 digit `endBy` (char 'A')) "123ABC123"

p0 :: Parsec [Char] u ([Char], [Char])
p0 = pure (,) <*> many1 letter <*> many1 digit

p1 :: Parsec [Char] u ([Char], [Char])
p1 = (,) <$> many1 letter <*> (many space *> many1 digit)


vowel :: Parsec [Char] u Char
vowel = oneOf "aeiou"

vowel1 :: Parsec [Char] u Char
vowel1 = oneOf "dbcvgs"
