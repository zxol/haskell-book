-- phone exersize, chapter 11 (page 480) of the haskell book.
-- models the alpha-numeric keypad commonly found on older mobile phones

import Data.List
import Data.Char
import Data.Function (on)

data Key = Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9 | KeyStar | KeyPound | KeyUnknown
  deriving (Eq, Show)
type NumPresses = Int

keymap :: [(Key, String)]
keymap = [
  (Key1, "1"),
  (Key2, "abc2"),
  (Key3, "def3"),
  (Key4, "ghi4"),
  (Key5, "jkl5"),
  (Key6, "mno6"),
  (Key7, "pqrs7"),
  (Key8, "tuv8"),
  (Key9, "wxyz9"),
  (KeyStar, "*"),
  (Key0, " 0"),
  (KeyPound, ".,#"),
  (KeyUnknown, "?")
  ]

keyLiteral :: Key -> Char
keyLiteral key = last $ snd (findKeyInKeymap key) -- the last character in the string is the literal character

findKeyInKeymap :: Key -> (Key, String)
findKeyInKeymap c = head $ filter ((c ==) . fst) keymap

findCharInKeymap :: Char -> (Key, String)
findCharInKeymap c = case find (elem c . snd) keymap of
                       Just key -> key
                       Nothing  -> (KeyUnknown, "?") -- if no char is found in the map, provide a special type

howToTypeCharacter :: Char -> [(Key, NumPresses)]
howToTypeCharacter c | isUpper c = [(KeyStar, 1)] ++ howToTypeCharacter (toLower c)
howToTypeCharacter c = [(fst key, pos)]
  where
    key = findCharInKeymap c
    pos
      | fst key == KeyUnknown = 1
      | otherwise = case elemIndex c (snd key) of
          Just p  -> p + 1
          Nothing -> error "key not found in keymap"

howToTypeMessage :: String -> [(Key, NumPresses)]
howToTypeMessage = concatMap howToTypeCharacter

howToTypeCharacterLiteral :: Char -> String
howToTypeCharacterLiteral c = concatMap go $ howToTypeCharacter c
  where
    go key = replicate (snd key) (keyLiteral (fst key))

howToTypeMessageLiteral :: String -> String
howToTypeMessageLiteral = concatMap howToTypeCharacterLiteral

howManyPressesFor :: String -> NumPresses
howManyPressesFor = foldl (\x y -> x + snd y) 0 . howToTypeMessage

mostPopular :: Ord a => [a] -> [a]
mostPopular = maximumBy (compare `on` length) . group . sort

mostPopularLetterGroup :: String -> String
mostPopularLetterGroup = mostPopular . filter (/=' ')

mostPopularLetterCount :: String -> Int
mostPopularLetterCount = length . mostPopularLetterGroup

mostPopularLetterCost :: String -> Int
mostPopularLetterCost = howManyPressesFor . mostPopularLetterGroup

mostPopularLetter :: String -> Char
mostPopularLetter = head . mostPopularLetterGroup

coolestLetter :: [String] -> Char
coolestLetter = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = head . mostPopular . words . concat

-- Testing

convo :: [String]
convo =
  ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

convoTestLiteral :: [String] -> [String]
convoTestLiteral = map howToTypeMessageLiteral

main =
  do
    print "hello"
