module Main where

import ExprT
import Parser
import StackVM

eval :: ExprT -> Integer
eval (Lit n) = n
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit ExprT.Add ExprT.Mul s of
  Just e -> Just (eval e)
  Nothing -> Nothing

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = ExprT.Add
  mul = ExprT.Mul

main :: IO ()
main = putStrLn $ show $ eval (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT)
