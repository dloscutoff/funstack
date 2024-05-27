module VerboseParser (
  parse,
  parseArgs
) where

import Data.Char (isUpper, isLower, toUpper)
import qualified Data.Map as Map
import Text.Read (readMaybe)
import Value (Value (..), chr')
import Command (Command (..))
import qualified BuiltinFunction as BF
import qualified BuiltinModifier as BM

-- Parse a string as a built-in function, or Nothing if parsing fails
-- The first several cases are aliases; if none of those match, default to
-- BuiltinFunction's Read instance
parseFunction :: String -> Maybe BF.BuiltinFunction
parseFunction s = case s of
  "All?" -> Just BF.All
  "Any?" -> Just BF.Any
  "Chunk" -> Just BF.Chunks
  "ChunksOf" -> Just BF.Chunks
  "Different" -> Just BF.NotSame
  "Different?" -> Just BF.NotSame
  "Equal?" -> Just BF.Equal
  "Falsey" -> Just BF.Not
  "Falsey?" -> Just BF.Not
  "Greater?" -> Just BF.Greater
  "GreaterEqual?" -> Just BF.GreaterEqual
  "Less?" -> Just BF.Less
  "LessEqual?" -> Just BF.LessEqual
  "Mod2" -> Just BF.Parity
  "Negative?" -> Just BF.Negative
  "NotEqual?" -> Just BF.NotEqual
  "NotSame?" -> Just BF.NotSame
  "Odd" -> Just BF.Parity
  "Odd?" -> Just BF.Parity
  "Positive?" -> Just BF.Positive
  "Prefixes" -> Just BF.Inits
  "Same?" -> Just BF.Same
  "Suffixes" -> Just BF.Tails
  "Unique" -> Just BF.Nub
  "Uniquify" -> Just BF.Nub
  "Zero?" -> Just BF.Zero
  _ -> readMaybe s

-- Parse a string as a built-in modifier, or Nothing if parsing fails
-- The first several cases are aliases; if none of those match, default to
-- BuiltinModifier's Read instance
parseModifier :: String -> Maybe BM.BuiltinModifier
parseModifier s = case s of
  "map" -> Just BM.Mapzip
  "mapflat" -> Just BM.Flatmap
  "mapflatten" -> Just BM.Flatmap
  "zipwith" -> Just BM.Mapzip
  _ -> readMaybe $ capitalize s
  where
    capitalize (c : cs) = toUpper c : cs
    capitalize "" = ""

{-
-- Parse a string as a stack command, or Nothing if parsing fails
parseStackCommand :: String -> Maybe (Stack -> Stack)
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
--  Character, if it's \ followed by a decimal char code
--  Otherwise, fall back on the Read instance of Value
parseLiteral :: String -> Maybe Value
parseLiteral s
  | Map.member s specialValues = Map.lookup s specialValues
  | ('\\' : n) <- s = (Character . chr') <$> readMaybe n
  | otherwise = readMaybe s

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
parseCommand :: String -> Either String Command
parseCommand "" = Left "Empty token"
parseCommand s = maybeToEither errorMessage command
  where
    errorMessage = "While parsing, unrecognized token: " ++ s
    c = head s
    command
      | isUpper c = PushFn <$> parseFunction s
      | isLower c = ModifyFn <$> parseModifier s
--      | c == '!' = StackCmd <$> parseStackCommand s
      | c == '@' = BindArg <$> readMaybe (tail s)
      | otherwise = BindVal <$> parseLiteral s

-- Split a line of code into tokens
-- For the moment, just split on spaces
-- TODO: a better strategy that can handle spaces in string literals
tokenizeLine :: String -> [String]
tokenizeLine = words

-- Parse a line of code as a list of Commands
parseLine :: String -> Either String [Command]
parseLine = mapM parseCommand . tokenizeLine

-- Parse a full program as a list of lists of Commands
parse :: String -> Either String [[Command]]
parse = mapM parseLine . lines

-- Parse an argument as either a Value or an error message
parseArg :: String -> Either String Value
parseArg a = maybeToEither ("Could not parse argument " ++ a) (readMaybe a)

-- Parse a list of arguments as a list of Values or an error message
parseArgs :: [String] -> Either String [Value]
parseArgs = mapM parseArg
