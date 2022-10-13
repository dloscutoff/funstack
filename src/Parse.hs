module Parse (
  parse,
  parseArgs
) where

import Data.Char (isDigit, isUpper, isLower)
import qualified Data.Map as Map
import Text.Read (readMaybe)
import Value (Value (..), chr', stringToVal)
import Function (Function)
import Modifier (Modifier)
import Command (Command (..))
import BuiltinFunctions (builtins)
import BuiltinModifiers (modifiers)

-- Parse a string as a builtin function, or Nothing if parsing fails
toFunction :: String -> Maybe Function
toFunction s = Map.lookup s builtins

-- Parse a string as a builtin modifier, or Nothing if parsing fails
toModifier :: String -> Maybe Modifier
toModifier s = Map.lookup s modifiers

{-
-- Parse a string as a stack command, or Nothing if parsing fails
toStackCommand :: String -> Maybe (Stack -> Stack)
toStackCommand s = Map.lookup s stackCommands
-}

-- Constants that aren't number/character/string/list literals
specialValues :: Map.Map String Value
specialValues = Map.fromList [
  ("\\t", Character '\t'),
  ("\\n", Character '\n'),
  ("\\s", Character ' '),
  ("$A", List $ map Character ['A'..'Z']),
  ("$a", List $ map Character ['a'..'z']),
  ("$Aa", List $ map Character $ ['A'..'Z'] ++ ['a'..'z']),
  ("$0", List $ map Character ['0'..'9']),
  ("$P", List $ map Character [' '..'~']),
  ("#N", List $ map Number [0..]),
  ("#N+", List $ map Number [1..]),
  ("#Z", List $ map Number $ [1..] >>= (\n -> [1-n, n]))
  ]

-- Parse a string as a literal Value, or Nothing if parsing fails
--  Special value, if it's in the list of special values
--  Character, if it's \ followed by a char code or any single character
--   surrounded by ''
--  String, if it starts with "
--  Number, if it starts with a digit or minus sign
toLiteral :: String -> Maybe Value
toLiteral s
  | Map.member s specialValues = Map.lookup s specialValues
  | ('\\' : n) <- s = (Character . chr') <$> readMaybe n
  | ('\'' : c : "'") <- s = Just $ Character c
  | head s == '"' = stringToVal <$> readMaybe s
  | (all isDigit s ||
     head s == '-' && all isDigit (tail s)) = Number <$> readMaybe s
  | otherwise = Nothing

-- Given an error message and a Maybe, return an Either String
--  If the second argument is Just x, return Right x
--  If the second argument is Nothing, return Left message
maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _ (Just x) = Right x
maybeToEither message Nothing = Left message

-- Parse a string as a Command, or (Left errorMessage) if parsing fails
--  Starts with an uppercase letter: builtin function
--  Starts with a lowercase letter: builtin modifier
--  Starts with !: stack command
--  Starts with @: argument reference
--  Otherwise: literal value
toCommand :: String -> Either String Command
toCommand "" = Left "Empty token"
toCommand s = maybeToEither errorMessage command
  where
    errorMessage = "While parsing, unrecognized token: " ++ s
    c = head s
    command
      | isUpper c = PushFn <$> toFunction s
      | isLower c = ModifyFn <$> toModifier s
--      | c == '!' = StackCmd <$> toStackCommand s
      | c == '@' = BindArg <$> readMaybe (tail s)
      | otherwise = BindVal <$> toLiteral s

-- Split a line of code into tokens
-- For the moment, just split on spaces
-- TODO: a better strategy that can handle spaces in string literals
tokenizeLine :: String -> [String]
tokenizeLine = words

-- Parse a line of code as a list of Commands
parseLine :: String -> Either String [Command]
parseLine = mapM toCommand . tokenizeLine

-- Parse a full program as a list of lists of Commands
parse :: String -> Either String [[Command]]
parse = mapM parseLine . lines

-- Parse an argument as either a Value or an error message
parseArg :: String -> Either String Value
parseArg a = maybeToEither ("Could not parse argument " ++ a) (toLiteral a)

-- Parse a list of arguments as a list of Values or an error message
parseArgs :: [String] -> Either String [Value]
parseArgs = mapM parseArg
