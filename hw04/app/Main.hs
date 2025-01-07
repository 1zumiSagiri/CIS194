module Main where

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . map (\x -> if even x then x else x * 2) . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node h l y r)
      | height l <= height r = let l' = insert x l in Node (height l' + 1) l' y r
      | otherwise = let r' = insert x r in Node (height r' + 1) l y r'
    height Leaf = -1
    height (Node h _ _ _) = h


xor :: [Bool] -> Bool
xor = foldr (\x y -> if x then not y else y) False

main :: IO ()
main = putStrLn "Hello, Haskell!"
