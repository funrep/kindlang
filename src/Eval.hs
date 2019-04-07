module Eval where

import Control.Monad (liftM2)
import Data.Maybe (catMaybes)
import Data.Bifunctor (bimap)

import Types

eval :: Expr -> Maybe Expr
eval (Var v)              = undefined
eval n@(Int _)            = Just n
eval b@(Bool _)           = Just b
eval (Not e)              =
  case eval e of
    Just (Bool b) -> Just . Bool $ not b
    _ -> Nothing
eval (List xs)            = Just . List . catMaybes $ map eval xs
eval (BinOp o)            = evalBinOp o
eval (IfElse cond a b)    =
  case eval cond of
    Just (Bool True)  -> eval a
    Just (Bool False) -> eval b
    _ -> Nothing
eval (Lambda param body)  = undefined
eval (App f arg)          = apply f arg
eval (Let var exp body)   = undefined

evalBinOp :: BinOp Expr -> Maybe Expr
evalBinOp (Add a b) = case evalBoth a b of
  Just (Int x, Int y) -> Just . Int $ x + y
  _ -> Nothing
evalBinOp (Sub a b) = case evalBoth a b of
  Just (Int x, Int y) -> Just . Int $ x - y
  _ -> Nothing
evalBinOp (Mul a b) = case evalBoth a b of
  Just (Int x, Int y) -> Just . Int $ x * y
  _ -> Nothing
evalBinOp (Div a b) = case evalBoth a b of
  Just (Int x, Int y) -> Just . Int $ x `div` y
  _ -> Nothing
evalBinOp (Mod a b) = case evalBoth a b of
  Just (Int x, Int y) -> Just . Int $ x `mod` y
  _ -> Nothing
evalBinOp (Less a b) = case evalBoth a b of
  Just (Bool x, Bool y) -> Just . Bool $ x < y
  _ -> Nothing
evalBinOp (LessEq a b) = case evalBoth a b of
  Just (Bool x, Bool y) -> Just . Bool $ x <= y
  _ -> Nothing
evalBinOp (Great a b) = case evalBoth a b of
  Just (Bool x, Bool y) -> Just . Bool $ x > y
  _ -> Nothing
evalBinOp (GreatEq a b) = case evalBoth a b of
  Just (Bool x, Bool y) -> Just . Bool $ x >= y
  _ -> Nothing
evalBinOp (Equal a b) = case evalBoth a b of
  Just (Bool x, Bool y) -> Just . Bool $ x == y
  _ -> Nothing
evalBinOp (NotEqual a b) = case evalBoth a b of
  Just (Bool x, Bool y) -> Just . Bool $ x /= y
  _ -> Nothing
evalBinOp (And a b) = case evalBoth a b of
  Just (Bool x, Bool y) -> Just . Bool $ x && y
  _ -> Nothing
evalBinOp (Or a b) = case evalBoth a b of
  Just (Bool x, Bool y) -> Just . Bool $ x || y
  _ -> Nothing
 
evalBoth :: Expr -> Expr -> Maybe (Expr, Expr)
evalBoth = (((uncurry $ liftM2 (,)) . bimap eval eval) .) . (,)

apply :: Expr -> Expr -> Maybe Expr
apply f arg = case eval f of
  _ -> Nothing
