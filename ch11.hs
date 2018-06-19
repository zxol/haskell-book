import Data.Char
import Data.String

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert b Leaf = Node Leaf b Leaf
insert b (Node left a right)
    | b == a = Node left a right
    | b <  a = Node (insert b left) a right
    | b >  a = Node left a (insert b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)
testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

data Expr = Lit Integer | Add Expr Expr
eval :: Expr -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b

asPatternTest :: (Int, Int) -> Int
asPatternTest n@(a,b) = a * b

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf a@(x:xs) (y:ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf a ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords s = map go $ words s
  where
    go s@(x:xs) = (s, toUpper x : xs)


-- testingHlint :: Int -> Int -> Bool
-- testingHlint a b = (a -- b)

main =
  do
    print "hello"
