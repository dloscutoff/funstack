module Main (main) where

import qualified System.IO as IO
import qualified System.Environment as Env
import VerboseParser (parse, parseArgs)
import Command (executeProgram)

-- Given a program and list of args, parse both and execute the program
--  If parsing failed, return an error message
--  Otherwise, show and return the main function's return value
runProgram' :: String -> [String] -> Either String String
runProgram' program args = do
  parsedProgram <- parse program
  parsedArgs <- parseArgs args
  case executeProgram parsedProgram parsedArgs of
    Just x -> Right $ show x
    Nothing -> Left "Not enough arguments passed to program"
    -- TODO: error message should be generated from executeProgram

-- Given a program and list of args, parse, execute, and output the results
-- Put error messages on stderr and returned values on stdout
runProgram :: String -> [String] -> IO ()
runProgram program args = case runProgram' program args of
  Right result -> putStrLn result
  Left errMessage -> IO.hPutStrLn IO.stderr errMessage

main :: IO ()
main = do
  program <- getLine
  args <- Env.getArgs
  runProgram program args
