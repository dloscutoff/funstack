module VerboseParser (
  parseProgram,
  parseArgs
) where

import Data.Char (isUpper, isLower, isDigit, isSpace)
import qualified Data.Map as Map
import Text.Read (readPrec, readMaybe)
import Text.ParserCombinators.ReadPrec (
  ReadPrec,
  lift,
  get,
  (<++),
  pfail,
  choice
  )
import qualified Text.ParserCombinators.ReadP as ReadP
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

data TokenList = TokenList {
  tokensOrError :: Either String [Token]
}

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

-- Helper ReadPrec parsers for the Read instances below:

-- Skip over a string of as many whitespace characters as possible
skipWhitespace :: ReadPrec ()
skipWhitespace = lift ReadP.skipSpaces

-- Match a string of as many non-whitespace characters as possible
getNonSpaceString :: ReadPrec String
getNonSpaceString = lift $ ReadP.munch1 $ not . isSpace

-- Match a string of as many digits as possible
getDigitString :: ReadPrec String
getDigitString = lift $ ReadP.munch1 isDigit

-- Match the rest of the string unconditionally
getWholeString :: ReadPrec String
getWholeString = lift $ ReadP.munch $ const True

-- To read a Token, read a built-in function or modifier, an argument
-- reference, a literal, a function or modifier alias, or a special value
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
      -- Match a built-in function's name exactly
      readFunction = do
        name@(firstChar : _) <- getNonSpaceString
        if isUpper firstChar
        then case readMaybe name of
          Just f -> pure (Function f)
          Nothing -> pfail
        else pfail
      -- Match a built-in modifier's name exactly (but with the first letter
      -- in lowercase)
      readModifier = do
        name@(firstChar : _) <- getNonSpaceString
        if isLower firstChar
        then case readMaybe (capitalise name) of
          Just m -> pure (Modifier m)
          Nothing -> pfail
        else pfail
      -- Match an argument reference like @1
      readArgReference = do
        '@' <- get
        argNumber <- getDigitString
        case readMaybe argNumber of
          Just n -> pure (Argument n)
          Nothing -> pfail
      -- Match a number/character/string/list literal
      readLiteral = Literal <$> readPrec
      -- Match a character code literal like \13
      readCharCodeLiteral = do
        '\\' <- get
        charCode <- getDigitString
        case readMaybe charCode of
          Just n -> pure (Literal $ Character $ chr' n)
          Nothing -> pfail
      -- Match an alias for a built-in function
      readFunctionAlias = do
        tokenString <- getNonSpaceString
        case Map.lookup tokenString functionAliases of
          Just f -> pure (Function f)
          Nothing -> pfail
      -- Match an alias for a built-in modifier
      readModifierAlias = do
        tokenString <- getNonSpaceString
        case Map.lookup tokenString modifierAliases of
          Just m -> pure (Modifier m)
          Nothing -> pfail
      -- Match a special value like #N
      readSpecialValue = do
        tokenString <- getNonSpaceString
        case Map.lookup tokenString specialValues of
          Just v -> pure (SpecialValue v)
          Nothing -> pfail

-- To read a TokenList, skip leading whitespace and then either:
--  Successfully read a Token and continue parsing recursively
--  Hit a bad token and return an error
--  Hit the end of input and return success
instance Read TokenList where
  readPrec = do
    _ <- skipWhitespace
    choice [
      readNextToken <++ badTokenError,
      endOfInput
      ] where
        -- Read a token; recursively read the rest of the token list, and
        -- either prepend this token to the list or pass through the error
        -- message
        readNextToken = do
          token <- readPrec
          TokenList result <- readPrec
          pure (TokenList $ (token :) <$> result)
        -- Failed to read a token; generate an error message using the
        -- next run of non-space characters and skip the rest of the
        -- string
        badTokenError = do
          badToken <- getNonSpaceString
          _ <- getWholeString
          -- TODO: More granular error messages depending on what badToken
          -- is? E.g. "Unterminated string literal" if it starts with a quote
          pure (TokenList $ Left $ "While scanning, unrecognized token: " ++ badToken)
        -- If we are at the end of input, return a successful empty token list
        endOfInput = pure (TokenList $ Right [])

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

-- Scan a full program as a list of Tokens by reading a TokenList and
-- then extracting either a list of tokens or error message from it
scanProgram :: String -> Either String [Token]
scanProgram code = case readMaybe code of
  Just tokenList -> tokensOrError tokenList
  -- This case shouldn't happen, but handle it gracefully
  Nothing -> Left "Error while scanning program"

-- Parse a full program as a list of lists of Commands
parseProgram :: String -> Either String [[Command]]
parseProgram = (parseTokens =<<) . scanProgram

-- Parse an argument as either a Value or an error message
parseArg :: String -> Either String Value
parseArg arg = maybeToEither ("Could not parse argument " ++ arg) (readMaybe arg)

-- Parse a list of arguments as a list of Values or an error message
parseArgs :: [String] -> Either String [Value]
parseArgs = mapM parseArg
