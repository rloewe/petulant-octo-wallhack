module Main where

import Interpreter
import PowAST
import qualified Parse as P
import qualified Data.Map as M
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = runMoreCode M.empty M.empty

runMoreCode :: SymTab -> FunTab -> IO ()
runMoreCode vtab ftab = do
  putStr "> "
  input <- getLine
  let ast = parse P.expr "" input
  case ast of
    Right ast' -> do
      (_, vtab') <- evalExpr vtab ftab (Write ast')
      putStr "\n"
      runMoreCode vtab' ftab
    Left e -> do
      putStrLn $ show e
      putStr "\n"
      runMoreCode vtab ftab
