
module Parse where

import Text.ParserCombinators.Parsec
import PowAST

-- We have many fun !!
program :: Parser Program
program = many fun

fun :: Parser Fun
fun = do
  sc <- scoping
  fn <- funName
  _  <- char '<'
  ps <- params
  _  <- char '>'
  _  <- char '/'
  bd <- funBody
  _  <- char '\\'
  return $ Fun fn sc ps bd
