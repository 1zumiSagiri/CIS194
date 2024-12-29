module Main where

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = x + sumDigits xs

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = [n `mod` 10] ++ toDigits (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
-- double every other element from the right
doubleEveryOther (x : y : xs) = x : y * 2 : doubleEveryOther xs

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

main :: IO ()
main = do
  print $ validate 4012888888881881
  print $ validate 4012888888881882
