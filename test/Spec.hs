import Test.Hspec

import Text.Parsec

import KindLang
import Types

programs =
  [ "let f = lambda x -> x + x in f 2"
  , "let x = True in if x then 1 else 0"
  , "420 * 1337 / 85"
  ]

rightJust :: Expr -> Either ParseError (Maybe Expr)
rightJust = Right . Just

main :: IO ()
main = hspec $ do
  describe "Program 1" $ do
    it "returns 2 + 2 = 4" $ do
      runProgram (programs !! 0) `shouldBe` rightJust (Int 4)
    it "returns 1 if x is true" $ do
      runProgram (programs !! 1) `shouldBe` rightJust (Int 1)
    it "returns 420 * 1337 / 85 = 6606" $ do
      runProgram (programs !! 1) `shouldBe` rightJust (Int 6606)

  

