module Lib where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec

import Types
import Parser
import Eval

someFunc :: IO ()
someFunc = putStrLn "someFunc"

runProgram :: String -> Either ParseError (Maybe Expr)
runProgram = fmap eval . parseString . T.pack