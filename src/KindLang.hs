module KindLang where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

import Text.Parsec

import Types
import Parser
import Eval

runProgram :: String -> IO (Maybe Expr)
runProgram s = do
  case parseString $ T.pack s of 
    Right exp -> do
      res <- runExceptT $ eval stdlib exp
      case res of
        Right e -> return $ Just e
        Left runError -> do
          putStrLn $ show runError
          return Nothing
    Left parseErr -> do
      putStrLn $ show parseErr
      return Nothing

