module Types where

import Control.Monad.Except (ExceptT)
import Data.Text (Text)
import Data.Map (Map)

type Symbol = Text

type Program = [Expr]

type Eval = ExceptT Error IO

type Env = Map Symbol Expr

data Expr
  = Unit
  | Var Symbol
  | Int Integer
  | Bool Bool
  | Not Expr
  | List [Expr]
  | BinOp (BinOp Expr)
  | IfElse Expr Expr Expr
  | Lambda Symbol Expr
  | Action Action
  | App Expr Expr
  | Let Symbol Expr Expr
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data Action
  = PrintLn
  | ReadInt
  deriving (Show, Eq)

data Error
  = TypeError
  | UnboundVar
  | IOError
  | MiscError
  deriving (Show, Eq)
