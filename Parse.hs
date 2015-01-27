
module Parse where

import Control.Applicative ((<*))
import Data.Char
import Text.ParserCombinators.Parsec
import qualified PowAST as Ast

-- We have many fun !!
program :: Parser Ast.Program
program = do
  funs <- many fun
  _    <- eof
  return funs

fun :: Parser Ast.Fun
fun = do
  sc <- lexeme $ scoping
  fn <- lexeme $ funName
  _  <- lexeme $ char '<'
  ps <- lexeme $ params
  _  <- lexeme $ char '>'
  _  <- lexeme $ char '/'
  bd <- lexeme $ funBody
  _  <- lexeme $ char '\\'
  return $ Ast.Fun fn sc ps bd

scoping :: Parser Ast.Scoping
scoping = do
  scope <- symbol "static" <|> symbol "dynamic"
  return $ if scope == "static" then Ast.StaticScoping else Ast.DynamicScoping

name :: Parser String
name = do
  -- [a-zA-Z_][a-zA-Z0-9_]*
  z <- satisfy (orUnderscore isAlpha)
  y <- many $ satisfy (orUnderscore isAlphaNum)
  return $ [z] ++ y

funName :: Parser Ast.FunName
funName = name

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

symbol :: String -> Parser String
symbol = lexeme . string

params :: Parser [Ast.Param]
params = identifier `sepBy` (lexeme $ char ',')

identifier :: Parser Ast.Id
identifier = lexeme $ do
  _ <- char '~'
  id <- name
  return id

funBody :: Parser [Ast.Statement]
funBody = many statement

statement :: Parser Ast.Statement
statement = do
  e <- expr
  _ <- lexeme $ char ':'
  return e

expr :: Parser Ast.Expr
expr = numbers <|> giveback <|> assignment
  where
    numbers = do
      ns <- many1 digit
      return $ Ast.Constant $ Ast.Integer (read ns)
    giveback = do
      _ <- symbol "giveback" <|> symbol "↜"
      expr' <- expr
      return $ Ast.GiveBack expr'
    assignment = do
      id <- identifier
      _  <- symbol "←"
      value <- expr
      return $ Ast.Assign id value

orUnderscore :: (Char -> Bool) -> Char -> Bool
orUnderscore f c = f c || c == '_'
