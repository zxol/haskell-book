import Data.Bool

alltrue = [True, True, True, True]
allfalse = [True, True, True, True]
halfhalf = [True, False, True, False]

-- direct recursion, using (&&)
myAnd :: [Bool] -> Bool
myAnd = foldl (&&) True

myOr :: [Bool] -> Bool
myOr = foldl (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem el = myAny (== el)

myReverse :: [a] -> [a]
myReverse = foldl (\x -> (:x)) []

squish :: [[a]] -> [a]
squish = foldl (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if f x y == GT then x else y) (head xs) xs
