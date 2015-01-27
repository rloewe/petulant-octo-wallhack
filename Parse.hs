
module Parse where

import Control.Applicative hiding (many, (<|>))
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
scoping = (symbol "static" >> return Ast.StaticScoping) <|>
          (symbol "dynamic" >> return Ast.DynamicScoping)

name :: Parser String
name = do
  -- [a-zA-Z_][a-zA-Z0-9_]*
  first <- satisfy (orUnderscore isAlpha)
  rest <- many $ satisfy (orUnderscore isAlphaNum)
  return $ [first] ++ rest

funName :: Parser Ast.FunName
funName = name

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

symbol :: String -> Parser String
symbol = lexeme . string

params :: Parser [Ast.Param]
params = sepByCommas identifier

arguments :: Parser Ast.Args
arguments = sepByCommas expr

sepByCommas :: Parser a -> Parser [a]
sepByCommas = (flip sepBy) (symbol ",")

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
  _ <- symbol ":"
  return e

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

expr :: Parser Ast.Expr
expr =
      lessgreater
  <|> plusminus
  <|> timesdivide
  <|> giveback
  <|> assignment
  <|> call
  <|> numbers
  <|> variable
  -- <|> conditional
  where
    lessgreater = chainl1 plusminus binOp
                    -- how to handle same look ahead?!
      where binOp = -- (symbol ">=" >> return Ast.GreaterEq) <|>
                    -- (symbol "<=" >> return Ast.LessEq) <|>
                    (symbol "<" >> return Ast.Less) <|>
                    (symbol ">" >> return Ast.Greater)
    plusminus = chainl1 timesdivide binOp
      where binOp = (symbol "+" >> return Ast.Plus) <|>
                    (symbol "-" >> return Ast.Minus)
    timesdivide = chainl1 parseRealThing binOp
      where binOp = (symbol "*" >> return Ast.Times) <|>
                    (symbol "/" >> return Ast.Divide)
            parseRealThing = (parens expr) <|> numbers
    variable = do
      id <- identifier
      return $ Ast.Var id
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
    call = do
      symbol "<"
      args <- arguments
      symbol ">"
      symbol "↝"
      funName <- name
      return $ Ast.Call args funName

    conditional = do
      condition <- expr
      char '?'
      -- TODO: Pull this into `compound` parser
      char '/'
      thenBranch <- many statement
      char '\\'
      -- TODO: How to make else branch? Which may or may not be there?
      return $ Ast.If condition thenBranch []

orUnderscore :: (Char -> Bool) -> Char -> Bool
orUnderscore f c = f c || c == '_'
