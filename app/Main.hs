module Main (main) where

import qualified System.IO as IO
import qualified System.Environment as Env
import System.Directory (doesFileExist)
import VerboseParser (parseProgram, parseArgs)
import State (executeProgram)

-- Given a program and list of args, parse both and execute the program
--  If parsing failed, return an error message
--  Otherwise, show and return the main function's return value
runProgram' :: String -> [String] -> Either String String
runProgram' program args = do
  parsedProgram <- parseProgram program
  parsedArgs <- parseArgs args
  case executeProgram parsedProgram parsedArgs of
    Just x -> Right $ show x
    Nothing -> Left "Not enough arguments passed to program"
    -- TODO: error message should be generated from executeProgram

-- Output the result of the program, limiting the max length to prevent
-- infinite output
-- TODO: Command-line flag to configure this setting
outputResult :: String -> IO ()
outputResult s = do
  putStrLn truncatedOutput
  if length truncatedOutput == maxLength
    then IO.hPutStrLn IO.stderr "Output truncated."
    else return ()
  where
    maxLength = 10000
    truncatedOutput = take maxLength s

-- Given a program and list of args, parse, execute, and output the results
-- Put error messages on stderr and returned values on stdout
runProgram :: String -> [String] -> IO ()
runProgram program args = case runProgram' program args of
  Right result -> outputResult result
  Left errMessage -> IO.hPutStrLn IO.stderr errMessage

-- Load the program from the given file and return it
readProgramFromFile :: String -> IO String
readProgramFromFile = readFile

-- Prompt the user to enter a program on stdin and return it
readProgramFromStdin :: IO String
readProgramFromStdin = do
  putStrLn "Enter your program:"
  getLine

-- Given a list of args, either load the program from the file given in
-- the first arg, or read it from stdin; return the program and the
-- remaining args
loadProgram :: [String] -> IO (String, [String])
loadProgram allArgs@(firstArg : remainingArgs) = do
  validFilename <- doesFileExist firstArg
  program <- if validFilename then readProgramFromFile firstArg else readProgramFromStdin
  pure (program, if validFilename then remainingArgs else allArgs)
loadProgram [] = do
  program <- readProgramFromStdin
  pure (program, [])

main :: IO ()
main = do
  allArgs <- Env.getArgs
  (program, args) <- loadProgram allArgs
  runProgram program args
