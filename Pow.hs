module Main where

import Data.List
import Interpreter
import Parse
import System.Directory
import System.Environment
import Text.ParserCombinators.Parsec (parseFromFile)

doc :: [String]
doc = [ "Usage: pow <filename>"
      , ""
      , "  <filename> should end in .pow"
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] ->
      if ".pow" `isSuffixOf` filename
      then runFile filename
      else showDoc
    _ -> showDoc

showDoc :: IO ()
showDoc = mapM_ putStrLn doc

runFile :: String -> IO ()
runFile filename = do
  fileExists <- doesFileExist filename
  case fileExists of
    True -> do
      ast <- parseFromFile program filename
      case ast of
        Right ast' -> interpret ast'
        Left e -> putStrLn $ show e
    _ -> putStrLn $ (show filename) ++ " was not found"

