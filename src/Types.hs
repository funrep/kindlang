module Types where

import Data.Text (Text)

type Symbol = Text

data Expr
  = Var Symbol
  | Int Integer
  | Bool Bool
  | Not Expr
  | List [Expr]
  | BinOp (BinOp Expr)
  | IfElse Expr Expr Expr
  | Lambda Symbol Expr
  | App Expr Expr
  | Let Symbol Expr Expr
  deriving Show

data BinOp a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Mod a a
  | Less a a
  | LessEq a a
  | Great a a
  | GreatEq a a
  | Equal a a 
  | NotEqual a a
  | And a a
  | Or a a
  deriving Show
