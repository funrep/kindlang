{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Monad.Identity
import Control.Applicative ((<*), (*>), (<*>), (<$>), (<$))
import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec hiding (Empty)
import Text.Parsec.Text
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr

import Types

parser :: Parser Expr
parser = buildExpressionParser opTable expr

parseString :: Text -> Either ParseError Expr
parseString s = parse (parser <* eof) "" s

reservedNames = words "True False lambda if then else let in main"
reservedOpNames = words "-> && || ! + - * / % = < <= > >="

type TextLanguageDef st = Token.GenLanguageDef Text st Identity

lexerConfig :: TextLanguageDef st
lexerConfig = Token.LanguageDef
  { Token.commentStart    = "#-"
  , Token.commentEnd      = "-#"
  , Token.commentLine     = "#"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> char '_' <|> char '\''
  , Token.opStart         = Token.opLetter lexerConfig
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   = reservedNames
  , Token.reservedOpNames = reservedOpNames
  , Token.caseSensitive   = True
  }

lexer = Token.makeTokenParser lexerConfig

identifier = fmap T.pack $ Token.identifier lexer
symbol     = Token.symbol lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
brackets   = Token.brackets lexer
commaSep   = Token.commaSep lexer
integer    = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

binary name label assoc = Infix (do{ reservedOp name
                                   ; return (\x y -> label x y)
                                   }) assoc

prefix name label = Prefix (reservedOp name *> return (\x -> label x))

opTable = [ [ prefix "!" Not ]
          , [ application ]
          , [ binary "*" (binop Mul) AssocLeft
            , binary "/" (binop Div) AssocLeft
            , binary "%" (binop Mod) AssocLeft ]
          , [ binary "+" (binop Add) AssocLeft
            , binary "-" (binop Sub) AssocLeft
            ]
          , [ binary "==" (binop Equal) AssocLeft
            , binary "/=" (binop NotEqual) AssocLeft
            , binary "<" (binop Less) AssocLeft
            , binary "<=" (binop LessEq) AssocLeft
            , binary ">" (binop Great) AssocLeft
            , binary ">=" (binop GreatEq) AssocLeft
            ]
          , [ binary "&&" (binop And) AssocLeft ]
          , [ binary "||" (binop Or) AssocLeft ]
          ]
  where binop a x y = BinOp $ a x y

application = Infix space AssocLeft
  where space = whiteSpace *> notFollowedBy (choice . map reservedOp $ reservedOpNames) *> return (\x y -> App x y)

parseBinOpExpr :: Parser Expr
parseBinOpExpr = buildExpressionParser opTable parseTerm

parseInt :: Parser Expr
parseInt = Int <$> integer

parseBool :: Parser Expr
parseBool =
  Bool True <$ reserved "True"
    <|> Bool False <$ reserved "False"

parseVar :: Parser Expr
parseVar = Var <$> identifier

parseTerm :: Parser Expr
parseTerm = parseInt
  <|> parseBool
  <|> parseVar
  <|> parens expr

parseLet :: Parser Expr
parseLet = reserved "let" *> do
  s <- identifier
  reservedOp "="
  e <- expr
  reserved "in"
  e' <- expr
  return $ Let s e e'

parseLambda :: Parser Expr
parseLambda = reserved "lambda" *> ((\x y -> Lambda x y) <$> identifier <*> (reservedOp "->" *> expr))

parseIf :: Parser Expr
parseIf = reserved "if" *> ((\x y z -> IfElse x y z) <$> expr <*> (reserved "then" *> expr) <*> (reserved "else" *> expr))

expr :: Parser Expr
expr = parseLambda
  <|> parseLet
  <|> parseIf
  <|> parseBinOpExpr
  <|> parseTerm
