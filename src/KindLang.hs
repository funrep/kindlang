module KindLang where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

import Text.Parsec

import Types
import Parser
import Eval

runProgram :: String -> Either ParseError (Maybe Expr)
runProgram = fmap (eval M.empty) . parseString . T.pack
