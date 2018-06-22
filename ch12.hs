import Data.Maybe
import Data.Char

-- data Maybe a = Nothing | Just a

ifEvenAdd2 n = if even n then Just(n+2) else Nothing

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show


mkPersonM :: Name -> Age -> Maybe Person
mkPersonM name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

-- data Either a b = Left a | Right b

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name == "" = Left NameEmpty
  | age < 0 = Left AgeTooLow
  | otherwise = Right (Person name age)

notThe :: String -> Maybe String
notThe s
  | s == "the" = Nothing
  | otherwise = Just s

replaceThe :: String -> String
replaceThe s = unwords . map go $ words s
  where
    go w = case notThe w of
      Nothing -> "a"
      Just a -> a

ts = "the cat in the hat did a shat on the egg. the end."

isVowel :: Char -> Bool
isVowel c
  | toLower c == 'a' = True
  | toLower c == 'e' = True
  | toLower c == 'i' = True
  | toLower c == 'o' = True
  | toLower c == 'u' = True
isVowel _ = False

countF :: (a -> Bool) -> [a] -> Int
countF f xs = foldl (\acc a -> acc + fromEnum (f a) ) 0 xs

count :: (a -> Bool) -> [a] -> Int
count f xs = sum . map (fromEnum . f) xs

countTrue :: [Bool] -> Int
countTrue = count id

pairs :: [a] -> [(a,a)]
pairs a = zip (a) (tail a)

theBeforeVowel :: (String, String) -> Bool
theBeforeVowel ([],[]) = False
theBeforeVowel (a,[]) = False
theBeforeVowel (a,b)
  | a == "the" && (isVowel (head b)) = True
  | otherwise = False

countTheBeforeVowel :: String -> Int
countTheBeforeVowel s = countTrue beforeTheVowelPairs
  where
    beforeTheVowelPairs = map theBeforeVowel $ pairs (words s)

countTheBeforeVowel2 :: String -> Int
countTheBeforeVowel2 s = go (words s)
  where
    go (x:xs)
      | xs == [] = 0
      | x == "the" && (isVowel (head $ head xs)) = 1 + go xs
      | otherwise = go xs


main = do
  print "hello"
