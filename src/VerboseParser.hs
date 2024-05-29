module VerboseParser (
  parseProgram,
  parseArgs
) where

import Data.Char (isUpper, isLower, isDigit)
import qualified Data.Map as Map
import Text.Read (readPrec, readMaybe, Lexeme(Ident), lexP)
import Text.ParserCombinators.ReadPrec (get, look, pfail, choice)
import GHC.Utils.Misc (capitalise)
import Value (Value (..), chr')
import Command (Command (..))
import qualified BuiltinFunction as BF
import qualified BuiltinModifier as BM

data Token =
  Function BF.BuiltinFunction |
  Modifier BM.BuiltinModifier |
  Argument Int |
  Literal Value |
  SpecialValue Value
  deriving (Show)

-- Alternate names for built-in functions
functionAliases :: Map.Map String BF.BuiltinFunction
functionAliases = Map.fromList [
  ("All?", BF.All),
  ("Any?", BF.Any),
  ("Chunk", BF.Chunks),
  ("ChunksOf", BF.Chunks),
  ("Different", BF.NotSame),
  ("Different?", BF.NotSame),
  ("Equal?", BF.Equal),
  ("Falsey", BF.Not),
  ("Falsey?", BF.Not),
  ("Greater?", BF.Greater),
  ("GreaterEqual?", BF.GreaterEqual),
  ("Less?", BF.Less),
  ("LessEqual?", BF.LessEqual),
  ("Mod2", BF.Parity),
  ("Negative?", BF.Negative),
  ("NotEqual?", BF.NotEqual),
  ("NotSame?", BF.NotSame),
  ("Odd", BF.Parity),
  ("Odd?", BF.Parity),
  ("Positive?", BF.Positive),
  ("Prefixes", BF.Inits),
  ("Same?", BF.Same),
  ("Suffixes", BF.Tails),
  ("Unique", BF.Nub),
  ("Uniquify", BF.Nub),
  ("Zero?", BF.Zero)
  ]

-- Alternate names for built-in modifiers
modifierAliases :: Map.Map String BM.BuiltinModifier
modifierAliases = Map.fromList [
  ("map", BM.Mapzip),
  ("mapflat", BM.Flatmap),
  ("mapflatten", BM.Flatmap),
  ("zipwith", BM.Mapzip)
  ]

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

instance Read Token where
  readPrec = choice [
    readFunction,
    readModifier,
    readArgReference,
    readLiteral,
    readCharCodeLiteral,
    readFunctionAlias,
    readModifierAlias,
    readSpecialValue
    ] where
      getAnyString = do
        string <- look
        match string where
          match (c : cs) = do
            _ <- get
            rest <- match cs
            pure (c : rest)
          match "" = pure ""
      getDigitString = do
        string <- look
        match string where
          match (c : cs)
            | isDigit c = do
              _ <- get
              rest <- match cs
              pure (c : rest)
            | otherwise = pfail
          match "" = pure ""
      readFunction = do
        Ident tokenString@(firstChar : _) <- lexP
        if isUpper firstChar
        then case readMaybe tokenString of
          Just f -> pure (Function f)
          Nothing -> pfail
        else pfail
      readModifier = do
        Ident tokenString@(firstChar : _) <- lexP
        if isLower firstChar
        then case readMaybe (capitalise tokenString) of
          Just m -> pure (Modifier m)
          Nothing -> pfail
        else pfail
      readArgReference = do
        '@' <- get
        argNumber <- getDigitString
        case readMaybe argNumber of
          Just n -> pure (Argument n)
          Nothing -> pfail
      readLiteral = Literal <$> readPrec
      readCharCodeLiteral = do
        '\\' <- get
        charCode <- getDigitString
        case readMaybe charCode of
          Just n -> pure (Literal $ Character $ chr' n)
          Nothing -> pfail
      readFunctionAlias = do
        tokenString <- getAnyString
        case Map.lookup tokenString functionAliases of
          Just f -> pure (Function f)
          Nothing -> pfail
      readModifierAlias = do
        tokenString <- getAnyString
        case Map.lookup tokenString modifierAliases of
          Just m -> pure (Modifier m)
          Nothing -> pfail
      readSpecialValue = do
        tokenString <- getAnyString
        case Map.lookup tokenString specialValues of
          Just v -> pure (SpecialValue v)
          Nothing -> pfail

-- Given an error message and a Maybe, return an Either String
--  If the second argument is Just x, return Right x
--  If the second argument is Nothing, return Left message
maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _ (Just x) = Right x
maybeToEither message Nothing = Left message

-- Parse a list of Tokens into a list of lists of Commands
-- For now, just put all the tokens in a single function
-- TODO: more-complex program structures
parseTokens :: [Token] -> Either String [[Command]]
parseTokens tokens = Right [map tokenToCommand tokens] where
  tokenToCommand (Function f) = PushFn f
  tokenToCommand (Modifier m) = ModifyFn m
  tokenToCommand (Argument a) = BindArg a
  tokenToCommand (Literal v) = BindVal v
  tokenToCommand (SpecialValue v) = BindVal v

-- Scan a full program as a list of Tokens
-- TODO: a better strategy that can handle spaces in string literals
scanProgram :: String -> Either String [Token]
scanProgram = mapM scanWord . words where
  scanWord word = maybeToEither ("While scanning, unrecognized token: " ++ word) (readMaybe word)

-- Parse a full program as a list of lists of Commands
parseProgram :: String -> Either String [[Command]]
parseProgram = (parseTokens =<<) . scanProgram

-- Parse an argument as either a Value or an error message
parseArg :: String -> Either String Value
parseArg a = maybeToEither ("Could not parse argument " ++ a) (readMaybe a)

-- Parse a list of arguments as a list of Values or an error message
parseArgs :: [String] -> Either String [Value]
parseArgs = mapM parseArg
