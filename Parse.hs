
module Parse where

import Text.ParserCombinators.Parsec
import qualified PowAST as Ast

-- We have many fun !!
program :: Parser Ast.Program
program = many fun

fun :: Parser Ast.Fun
fun = do
  sc <- scoping
  fn <- funName
  _  <- char '<'
  ps <- params
  _  <- char '>'
  _  <- char '/'
  bd <- funBody
  _  <- char '\\'
  return $ Ast.Fun fn sc ps bd

scoping :: Parser Ast.Scoping
scoping = do
  scope <- string "static" <|> string "dynamic"
  return $ if scope == "static" then Ast.StaticScoping else Ast.DynamicScoping

name :: Parser String
name = many $ oneOf "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM_"

funName :: Parser Ast.FunName
funName = name

params :: Parser [Ast.Param]
params = identifier `sepBy` char ','

identifier :: Parser Ast.Id
identifier = do
  _ <- char '~'
  id <- name
  return id

funBody :: Parser [Ast.Statement]
funBody = many statement

statement :: Parser Ast.Statement
statement = do
  e <- expr
  _ <- char ':'
  return e

expr :: Parser Ast.Expr
expr = undefined
