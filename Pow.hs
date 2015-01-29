module Main where

import Parse
import Interpreter
import System.Environment
import Text.ParserCombinators.Parsec (parseFromFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      ast <- parseFromFile program filename
      case ast of
        Right ast' -> interpret ast'
        Left e -> putStrLn $ show e
    _ -> putStrLn "Usage: pow <filename>"
