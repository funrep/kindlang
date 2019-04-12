{-# LANGUAGE OverloadedStrings #-}
module Eval where

import Control.Monad (liftM2, (>>=))
import Control.Monad.Except (throwError, liftIO)
import Data.Maybe (catMaybes)
import Data.Bifunctor (bimap)
import qualified Data.Map as M
import Text.Read (readMaybe)

import Types

stdlib :: Env
stdlib = M.fromList
  [ ("println", Action PrintLn)
  , ("readint", Action ReadInt)
  ]

eval :: Env -> Expr -> Eval Expr
eval e Unit                 = return Unit
eval e (Var v)              =
  case M.lookup v e of
    Just exp -> eval e exp
    _ -> throwError UnboundVar
eval e n@(Int _)            = return n
eval e b@(Bool _)           = return b
eval e (Not exp)            = do
  res <- eval e exp
  case res of
    Bool b -> return . Bool $ not b
    _ -> throwError TypeError
eval e (List xs)            = fmap List . sequence $ fmap (eval e) xs
eval e (BinOp o)            = evalBinOp e o
eval e (IfElse cond a b)    = do
  res <- eval e cond
  case res of
    Bool True  -> eval e a
    Bool False -> eval e b
    _ -> throwError TypeError
eval e l@(Lambda p body)    = return l
eval e a@(Action _)         = return a
eval e (App f arg)          = apply e f arg
eval e (Let var exp body)   = do
  res <- eval e exp
  let e' = M.insert var res e
  eval e' body

evalBinOp :: Env -> BinOp Expr -> Eval Expr
evalBinOp e (Add a b) = do
  res <- evalBoth e a b
  case res of
    (Int x, Int y) -> return . Int $ x + y
    _ -> throwError TypeError
evalBinOp e (Sub a b) = do
  res <- evalBoth e a b
  case res of
    (Int x, Int y) -> return . Int $ x - y
    _ -> throwError TypeError
evalBinOp e (Mul a b) = do
  res <- evalBoth e a b
  case res of
    (Int x, Int y) -> return . Int $ x * y
    _ -> throwError TypeError
evalBinOp e (Div a b) = do
  res <- evalBoth e a b
  case res of
    (Int x, Int y) -> return . Int $ x `div` y
    _ -> throwError TypeError
evalBinOp e (Mod a b) = do
  res <- evalBoth e a b
  case res of
    (Int x, Int y) -> return . Int $ x `mod` y
    _ -> throwError TypeError
evalBinOp e (Less a b) = do
  res <- evalBoth e a b
  case res of
    (Bool x, Bool y) -> return . Bool $ x < y
    _ -> throwError TypeError
evalBinOp e (LessEq a b) = do
  res <- evalBoth e a b
  case res of
    (Bool x, Bool y) -> return . Bool $ x <= y
    _ -> throwError TypeError
evalBinOp e (Great a b) = do
  res <- evalBoth e a b
  case res of
    (Bool x, Bool y) -> return . Bool $ x > y
    _ -> throwError TypeError
evalBinOp e (GreatEq a b) = do
  res <- evalBoth e a b
  case res of
    (Bool x, Bool y) -> return . Bool $ x >= y
    _ -> throwError TypeError
evalBinOp e (Equal a b) = do
  res <- evalBoth e a b
  case res of
    (Bool x, Bool y) -> return . Bool $ x == y
    _ -> throwError TypeError
evalBinOp e (NotEqual a b) = do
  res <- evalBoth e a b
  case res of
    (Bool x, Bool y) -> return . Bool $ x /= y
    _ -> throwError TypeError
evalBinOp e (And a b) = do
  res <- evalBoth e a b
  case res of
    (Bool x, Bool y) -> return . Bool $ x && y
    _ -> throwError TypeError
evalBinOp e (Or a b) = do
  res <- evalBoth e a b
  case res of
    (Bool x, Bool y) -> return . Bool $ x || y
    _ -> throwError TypeError
 
evalBoth :: Env -> Expr -> Expr -> Eval (Expr, Expr)
evalBoth e a b = (uncurry $ liftM2 (,)) (eval e a, eval e b)

apply :: Env -> Expr -> Expr -> Eval Expr
apply e f arg = do
  res <- eval e f
  exp <- eval e arg
  case res of
    Lambda p body ->
      let e' = M.insert p exp e
      in eval e' body
    Action PrintLn -> do
      liftIO . putStrLn $ show arg
      return Unit
    Action ReadInt -> do
      s <- liftIO getLine
      case readMaybe s of
        Just n -> return $ Int n
        _ -> throwError IOError
    _ -> throwError TypeError
