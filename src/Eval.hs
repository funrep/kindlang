module Eval where

import Control.Monad (liftM2, (>>=))
import Data.Maybe (catMaybes)
import Data.Bifunctor (bimap)
import qualified Data.Map as M

import Types

eval :: Env -> Expr -> Program
eval e (Var v)              = M.lookup v e >>= eval e
eval e n@(Int _)            = Just n
eval e b@(Bool _)           = Just b
eval e (Not exp)            =
  case eval e exp of
    Just (Bool b) -> Just . Bool $ not b
    _ -> Nothing
eval e (List xs)            = Just . List . catMaybes $ map (eval e) xs
eval e (BinOp o)            = evalBinOp e o
eval e (IfElse cond a b)    =
  case eval e cond of
    Just (Bool True)  -> eval e a
    Just (Bool False) -> eval e b
    _ -> Nothing
eval e l@(Lambda p body)    = Just l
eval e (App f arg)          = apply e f arg
eval e (Let var exp body)   =
  case eval e exp of
    Just exp ->
      let e' = M.insert var exp e
      in eval e' body
    _ -> Nothing

evalBinOp :: Env -> BinOp Expr -> Program
evalBinOp e (Add a b) = case evalBoth e a b of
  Just (Int x, Int y) -> Just . Int $ x + y
  _ -> Nothing
evalBinOp e (Sub a b) = case evalBoth e a b of
  Just (Int x, Int y) -> Just . Int $ x - y
  _ -> Nothing
evalBinOp e (Mul a b) = case evalBoth e a b of
  Just (Int x, Int y) -> Just . Int $ x * y
  _ -> Nothing
evalBinOp e (Div a b) = case evalBoth e a b of
  Just (Int x, Int y) -> Just . Int $ x `div` y
  _ -> Nothing
evalBinOp e (Mod a b) = case evalBoth e a b of
  Just (Int x, Int y) -> Just . Int $ x `mod` y
  _ -> Nothing
evalBinOp e (Less a b) = case evalBoth e a b of
  Just (Bool x, Bool y) -> Just . Bool $ x < y
  _ -> Nothing
evalBinOp e (LessEq a b) = case evalBoth e a b of
  Just (Bool x, Bool y) -> Just . Bool $ x <= y
  _ -> Nothing
evalBinOp e (Great a b) = case evalBoth e a b of
  Just (Bool x, Bool y) -> Just . Bool $ x > y
  _ -> Nothing
evalBinOp e (GreatEq a b) = case evalBoth e a b of
  Just (Bool x, Bool y) -> Just . Bool $ x >= y
  _ -> Nothing
evalBinOp e (Equal a b) = case evalBoth e a b of
  Just (Bool x, Bool y) -> Just . Bool $ x == y
  _ -> Nothing
evalBinOp e (NotEqual a b) = case evalBoth e a b of
  Just (Bool x, Bool y) -> Just . Bool $ x /= y
  _ -> Nothing
evalBinOp e (And a b) = case evalBoth e a b of
  Just (Bool x, Bool y) -> Just . Bool $ x && y
  _ -> Nothing
evalBinOp e (Or a b) = case evalBoth e a b of
  Just (Bool x, Bool y) -> Just . Bool $ x || y
  _ -> Nothing
 
evalBoth :: Env -> Expr -> Expr -> Maybe (Expr, Expr)
evalBoth e = (((uncurry $ liftM2 (,)) . bimap (eval e) (eval e)) .) . (,)

apply :: Env -> Expr -> Expr -> Program
apply e f arg = case eval e f of
  Just (Lambda p body) ->
    case eval e arg of
      Just exp ->
        let e' = M.insert p exp e
        in eval e' body
      _ -> Nothing
  _ -> Nothing
