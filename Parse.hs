
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
  sc <- lexeme scoping
  fn <- lexeme $ funName
  _  <- lexeme $ char '⊂'
  ps <- lexeme $ params
  _  <- lexeme $ char '⊃'
  bd <- lexeme compoundStatement
  return $ Ast.Fun fn sc ps bd

scoping :: Parser Ast.Scoping
scoping = (symbol "static" >> return Ast.StaticScoping) <|>
          (symbol "dynamic" >> return Ast.DynamicScoping)

name :: Parser String
name = do
  -- NOTE: Maybe allow name to start with numbers
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

statement :: Parser Ast.Statement
statement = do
  e <- expr
  _ <- symbol ":"
  return e

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

expr :: Parser Ast.Expr
expr =
  giveback <|> conditional <|> loop <|> binOpChain

  where
    binOpChain = assignment
    assignment = chainr1 lessgreater (symbol "←" >> return Ast.Assign)
    lessgreater = chainl1 strConcat binOp
      where binOp = (try $ symbol ">=" >> return Ast.GreaterEq) <|>
                    (try $ symbol "<=" >> return Ast.LessEq) <|>
                    (try $ symbol "<" >> return Ast.Less) <|>
                    (try $ symbol ">" >> return Ast.Greater)
    strConcat = chainl1 plusminus binOp
      where binOp = symbol "↔" >> return Ast.StrConcat
    plusminus = chainl1 timesdivide binOp
      where binOp = (symbol "+" >> return Ast.Plus) <|>
                    (symbol "-" >> return Ast.Minus)
    timesdivide = chainl1 parseRealThing binOp
      where binOp = (symbol "*" >> return Ast.Times) <|>
                    (symbol "/" >> return Ast.Divide)
            parseRealThing = (parens expr)
                             <|> numbers
                             <|> variable
                             <|> call
    variable = do
      id <- identifier
      return $ Ast.Var id
    numbers = do
      ns <- lexeme $ many1 digit
      return $ Ast.Constant $ Ast.Integer (read ns)
    giveback = do
      _ <- symbol "giveback" <|> symbol "↜"
      expr' <- expr
      return $ Ast.GiveBack expr'
    call = do
      symbol "⊂"
      args <- arguments
      symbol "⊃"
      symbol "↝"
      funName <- name
      return $ Ast.Call args funName
    loop = do
      symbol "⟳"
      condition <- expr
      symbol "?"
      body <- compoundStatement
      return $ Ast.While condition body
    -- TODO: Make this less ugly!
    conditional = try conditionalWithElse <|> conditionalWithoutElse
    conditionalWithoutElse = do
      symbol "¿"
      condition <- expr
      symbol "?"
      thenBranch <- compoundStatement
      return $ Ast.If condition thenBranch []
    conditionalWithElse = do
      symbol "¿"
      condition <- expr
      symbol "?"
      thenBranch <- compoundStatement
      symbol "!"
      elseBranch <- compoundStatement
      return $ Ast.If condition thenBranch elseBranch

compoundStatement :: Parser [Ast.Statement]
compoundStatement = do
  symbol "/"
  body <- many statement
  symbol "\\"
  return body

orUnderscore :: (Char -> Bool) -> Char -> Bool
orUnderscore f c = f c || c == '_'
