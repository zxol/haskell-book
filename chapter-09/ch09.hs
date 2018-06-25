import Data.Char

filterUppercase = filter (not . isUpper)

capitaliseFirstLetter [] = []
capitaliseFirstLetter (x:xs) = toUpper x : xs

capitaliseAll = map toUpper

pfHeadCaps = toUpper . head

-- direct recursion, using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = (a == x) || myElem a xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverseEfficient :: [a] -> [a]
myReverseEfficient = foldl (\x -> (:x)) []
-- myReverseEfficient xs = foldl (flip (:)) [] xs -- prelude implementation

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishEfficient :: [[a]] -> [a]
squishEfficient = foldl (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs)
