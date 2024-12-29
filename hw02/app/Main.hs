module Main where

import Data.List (uncons)
import Log

--  Parse a log message from a string.
-- `words` splits a string into a list of words.
-- `uncons` splits a list into its head and tail to `Just(head, tail)` or `Nothing` if the list is empty.
-- `read` converts a string to a value of a type, in this case `Int`.
-- `unwords` converts a list of words into a string.
parseMessage :: String -> LogMessage
parseMessage s = case uncons (words s) of
  Just ("I", ts : msg) -> LogMessage Info (read ts) (unwords msg)
  Just ("W", ts : msg) -> LogMessage Warning (read ts) (unwords msg)
  Just ("E", code : ts : msg) -> LogMessage (Error (read code)) (read ts) (unwords msg)
  _ -> Unknown s

--  Parse a log file from a string.
-- `lines` splits a string into a list of lines.
-- `map` applies a function to each element of a list.
-- `.`, the function composition operator, `f . g` is equivalent to `f(g(x))`.
parse :: String -> [LogMessage]
parse = map parseMessage . lines -- parse s = map parseMessage (lines s)

--  Insert a log message into a message tree.
-- If the log message is an `Unknown`, return the tree unchanged.
-- If the tree is a `Leaf`, return a `Node` with the log message as the root.
-- If the tree is a `Node`, compare the timestamp of the log message with the timestamp of the root log message.
-- If the timestamp of the log message < the timestamp of the root log message, insert the log message into the left subtree.
-- If the timestamp of the log message >= the timestamp of the root log message, insert the log message into the right subtree.
insert :: MessageTree -> LogMessage -> MessageTree
insert t@_ (Unknown _) = t
insert t@(Node _ (Unknown _) _) _ = t
insert Leaf m = Node Leaf m Leaf
insert (Node l m'@(LogMessage _ t' _) r) m@(LogMessage _ t _) =
  if t < t'
    then Node (insert l m) m' r
    else Node l m' (insert r m)

--  Build a message tree from a list of log messages.
-- `foldl` applies a function (`b` -> `a` -> `b`) to each element of a list, starting with an accumulator value of type `b`.
-- The initial accumulator value is `Leaf` as the message tree is empty.
build :: [LogMessage] -> MessageTree
build = foldl insert Leaf

--  In-order traversal of a message tree.
-- `inOrder` returns a list of log messages in ascending order of timestamp.
-- If the tree is a `Leaf`, return an empty list.
-- If the tree is a `Node`, return the concatenation of the in-order traversal of the left subtree, the root log message, and the in-order traversal of the right subtree.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

-- Check if a log message is an error message with a severity of at least 50.
isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error s) _ _) = s >= 50
isSevereError _ = False

-- Extract the messages from a list of log messages that are severe errors.
-- The function use `filter isSevereError` to keep only the log messages that are severe errors.
-- and then `map` to extract the message from each log message.
-- `\(LogMessage _ _ m) -> m` is a lambda function that extracts the message from a log message.

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ m) -> m) . filter isSevereError

-- `testParse` returns IO [LogMessage]
-- `mapM_ putStrLn` applies `putStrLn` to each element of a IO list and ignores the result.
-- It has type `[String] -> IO ()`.
-- `map show` applies `show` to each element of a list, converting it to a string.
-- `mapM_ putStrLn . map show` applies `show` to each element of a list and prints the result.
-- It has type `[a] -> IO ()`.
-- bind (>>=) has type IO a -> (a -> IO b) -> IO b
-- It chains the IO action `testParse parse 10 "error.log"` with the function `mapM_ putStrLn . map show`. The result is a single `IO ()` action.
main :: IO ()
main = do
  putStrLn "Test Parsing"
  testParse parse 10 "error.log" >>= mapM_ putStrLn . map show
  putStrLn "\nTest What Went Wrong"
  testWhatWentWrong parse whatWentWrong "error.log" >>= mapM_ putStrLn

-- `mapM_` Applies a monadic function (A function that produce a monadic effect like IO) to each element of a (IO) list and ignores the result. 
-- It has type `m [a] -> m ()`, the return `m ()` is the result of the last monadic action with no value. 
-- `mapM_` perform the monadic action for its side effect, in this case, printing the result of the monadic action `putStrLn` within the `IO` monad.