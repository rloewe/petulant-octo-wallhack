module Repl where

import Interpreter
import PowAST
import qualified Parse as P
import Text.ParserCombinators.Parsec (parse)

makeProgram :: Expr -> Program
makeProgram expr = [Fun { funName = "main"
                        , funScoping = StaticScoping
                        , funParams = []
                        , funBody = [expr]
                        }]

main :: IO ()
main = do
  putStr "> "
  input <- getLine
  let ast = parse P.expr "" input
  case ast of
    Right ast' -> interpret $ makeProgram ast'
    Left e -> putStrLn $ show e
  putStrLn ""
  main
