module Main where

-- Exercise 1 Hopscotch
-- `skips` takes a list of elements and returns a list of lists
--      where each list contains every nth element of the input list.
--  It first generates a list of numbers from 1 to the length of the input list,
--      the purpose of this list is to determine the value of n in the `skip` function.
--  The `map` function applies the `skip` function with input `ls` to each element of the list of numbers,
--      `skip` will filter the list with each int elem it receives.
skips :: [a] -> [[a]]
skips ls = map (skip ls) [1 .. length ls]
  where
    --  The `skip` function takes a list and an integer n,
    --      it drops the first n - 1 elements of the list
    --      so that the first element of the new list is the nth element of the original list.
    --      it then recursively calls itself with the new list and the same value of n,
    --      so that the new list contains every nth element of the original list.
    skip :: [a] -> Int -> [a]
    skip ls' n = case drop (n - 1) ls' of
      [] -> []
      hd : tl -> hd : skip tl n

-- Exercise 2 Local maxima
-- Extract 3 ints from list, put the middle one in the result list
--    if it is greater than the other two.
localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : tl)
  | y > x && y > z = y : localMaxima (y : z : tl)
  | otherwise = localMaxima (y : z : tl)
localMaxima _ = []

-- Exercise 3 Histogram
-- `counts` use `map` function to apply the lambda function to each element of the input list `ls`,
--      the lambda function counts the number of occurrences of the element in the list
--      by filtering the list with the element and taking the length of the result.
-- `maxCount` is the maximum value in the `counts` list, we will use it to determine the height of the histogram.
-- `rows` is a list of strings, each string is a row of the histogram.
-- The `map` function applies the `row` function to each element of the list of integers from `maxCount` to 1.
-- `unlines` concatenates the list of strings into a single string with newline characters between each string.
histogram :: [Integer] -> String
histogram ls =
  let counts = map (\n -> length $ filter (== n) ls) [0 .. 9]
      maxCount = maximum counts
      rows = map (row counts) [maxCount, maxCount - 1 .. 1]
      footer = "==========\n0123456789\n"
   in unlines $ rows ++ [footer]
  where
    --  The `row` function takes a list of counts and an integer n,
    --      it generates a row of the histogram where each element is a '*' if the count is greater than or equal to n.
    --  This `n` is the height of the histogram at that row, which will decrease by 1 for each row(in the `map` function).
    row :: [Int] -> Int -> String
    row counts n = map (\c -> if c >= n then '*' else ' ') counts

main :: IO ()
main = do
  putStrLn "Exercise 1: Skips"
  print $ skips "hello~"
  putStrLn "Exercise 2: Local maxima"
  print $ localMaxima [2, 9, 5, 6, 1]
  putStrLn "Exercise 3: histogram"
  putStr $ histogram [1, 1, 1, 5]
  putStr $ histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9]
