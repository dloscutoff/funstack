module VerboseParser (
  parseProgram,
  parseArgs
) where

import Data.Char (
  isSpace,
  isLower,
  isUpper,
  isAlpha,
  isAlphaNum,
  isDigit
  )
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Text.Read (readPrec, readMaybe, readEither)
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
import Number (Number)
import Value (Value (..), toValue, chr')
import Command (Command (..))
import qualified BuiltinFunction as BF
import qualified BuiltinModifier as BM
import qualified BuiltinStackOp as BSO
import qualified Interpolation as I

data Token =
  Function BF.BuiltinFunction |
  Interpolation I.Interpolation |
  Modifier BM.BuiltinModifier |
  StackOp BSO.BuiltinStackOp |
  Argument Int |
  Literal Value |
  SpecialValue Value |
  Comment String
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
  ("Even?", BF.Even),
  ("Falsey", BF.Not),
  ("Falsey?", BF.Not),
  ("Greater?", BF.Greater),
  ("GreaterEqual?", BF.GreaterEqual),
  ("Invert", BF.Recip),
  ("Less?", BF.Less),
  ("LessEqual?", BF.LessEqual),
  ("Mod2", BF.Parity),
  ("Negative?", BF.Negative),
  ("NotEqual?", BF.NotEqual),
  ("NotSame?", BF.NotSame),
  ("Odd?", BF.Odd),
  ("Positive?", BF.Positive),
  ("Prefixes", BF.Inits),
  ("Same?", BF.Same),
  ("Suffixes", BF.Tails),
  ("Unique", BF.Nub),
  ("Uniquify", BF.Nub),
  ("Unlines", BF.Lines),
  ("Zero?", BF.Zero)
  ]

-- Alternate names for built-in modifiers
modifierAliases :: Map.Map String BM.BuiltinModifier
modifierAliases = Map.fromList [
  ("map", BM.Mapzip),
  ("mapflat", BM.Flatmap),
  ("mapflatten", BM.Flatmap),
  ("treemap", BM.Treemapzip),
  ("treezip", BM.Treemapzip),
  ("zipwith", BM.Mapzip)
  ]

-- Constants that aren't number/character/string/list literals
specialValues :: Map.Map String Value
specialValues = Map.fromList [
  ("\\t", toValue '\t'),
  ("\\n", toValue '\n'),
  ("\\s", toValue ' '),
  ("$A", toValue ['A'..'Z']),
  ("$a", toValue ['a'..'z']),
  ("$Aa", toValue $ ['A'..'Z'] ++ ['a'..'z']),
  ("$0", toValue ['0'..'9']),
  ("$P", toValue [' '..'~']),
  ("#N", toValue naturals),
  ("#N1", toValue $ tail naturals),
  ("#Z", toValue $ naturals >>= (\n -> [-n, n+1]))
  ]
  where naturals = [0 :: Number ..]

-- Helper ReadPrec parsers for the Read instances below:

-- Skip over zero or more whitespace characters
-- ReadP calls this skipSpaces, but we'll call it skipWhitespace
-- because we want to use skipSpaces for skipping whitespace that
-- doesn't include newlines
skipWhitespace :: ReadPrec ()
skipWhitespace = lift ReadP.skipSpaces

-- Skip over zero or more non-newline whitespace characters
skipSpaces :: ReadPrec ()
skipSpaces = lift $ ReadP.munch (\c -> isSpace c && c /= '\n') >> pure ()

-- Skip over the rest of the string unconditionally
skipWholeString :: ReadPrec ()
skipWholeString = lift $ ReadP.munch (const True) >> pure ()

-- Match one or more digits
getDigits :: ReadPrec String
getDigits = lift $ ReadP.munch1 isDigit

-- Match a name: start with a letter, then 0 or more letters or numbers,
-- then optionally a trailing ?
getName :: ReadPrec String
getName = lift $ do
  c <- ReadP.satisfy isAlpha
  s <- ReadP.munch isAlphaNum
  q <- ReadP.string "?" ReadP.<++ ReadP.string ""
  pure (c : s ++ q)

-- Match a special value token: start with one of # $ \, then 1 or more
-- letters or numbers
getSpecialValue :: ReadPrec String
getSpecialValue = lift $ do
  c <- ReadP.choice (map ReadP.char "#$\\")
  s <- ReadP.munch1 isAlphaNum
  pure (c : s)

-- Match a string of as many non-whitespace characters as possible
-- (must be at least one)
getNonSpaceString :: ReadPrec String
getNonSpaceString = lift $ ReadP.munch1 $ not . isSpace

-- Match a backtick-wrapped string (interpolation literal)
-- Backticks cannot be used within the string unless escaped with \
getBacktickLiteral :: ReadPrec [String]
getBacktickLiteral = lift $ do
  '`' <- ReadP.get
  ss <- ReadP.many getAtom
  '`' <- ReadP.get
  pure ss
  where
    getAtom = ReadP.choice [
      do b <- ReadP.char '\\'; c <- ReadP.get; pure (b : c : ""),
      (: "") <$> ReadP.satisfy (`notElem` "`\\")
      ]

-- Match the rest of the string up to (but not including) the next newline
getRestOfLine :: ReadPrec String
getRestOfLine = lift $ ReadP.munch (/= '\n')

-- Match the end of input
eof :: ReadPrec ()
eof = lift $ ReadP.eof

-- To read a Token, read a built-in function, modifier, or stack operation;
-- an argument reference; a literal; a comment; a function or modifier alias;
-- or a special value
instance Read Token where
  readPrec = choice [
    readFunction,
    readModifier,
    readStackOp,
    readArgReference,
    readLiteral,
    readCharCodeLiteral,
    readInterpolation,
    readComment,
    readFunctionAlias,
    readModifierAlias,
    readSpecialValue
    ] where
      -- Match a built-in function's name exactly
      readFunction = do
        name@(firstChar : _) <- getName
        if isUpper firstChar
        then case readMaybe name of
          Just f -> pure (Function f)
          Nothing -> pfail
        else pfail
      -- Match a built-in modifier's name exactly (but with the first letter
      -- in lowercase)
      readModifier = do
        name@(firstChar : _) <- getName
        if isLower firstChar
        then case readMaybe (capitalise name) of
          Just m -> pure (Modifier m)
          Nothing -> pfail
        else pfail
      -- Match a built-in stack operator's name exactly (but with a leading !
      -- and the first letter in lowercase)
      readStackOp = do
        '!' <- get
        name@(firstChar : _) <- getName
        if isLower firstChar
        then case readMaybe (capitalise name) of
          Just o -> pure (StackOp o)
          Nothing -> pfail
        else pfail
      -- Match an argument reference like @1
      readArgReference = do
        '@' <- get
        argNumber <- getDigits
        case readMaybe argNumber of
          Just n -> pure (Argument n)
          Nothing -> pfail
      -- Match a number/character/string/list literal
      readLiteral = Literal <$> readPrec
      -- Match a character code literal like \13
      readCharCodeLiteral = do
        '\\' <- get
        charCode <- getDigits
        case readMaybe charCode of
          Just n -> pure (Literal $ toValue $ chr' n)
          Nothing -> pfail
      -- Match an interpolation string like `abc$xyz`
      readInterpolation = do
        components <- getBacktickLiteral
        case sequence $ map readMaybe components of
          Just i -> pure $ Interpolation $ I.condense i
          Nothing -> pfail
      -- Match a line comment starting with ;
      readComment = do
        ';' <- get
        skipSpaces
        Comment <$> getRestOfLine
      -- Match an alias for a built-in function
      readFunctionAlias = do
        alias <- getName
        case Map.lookup alias functionAliases of
          Just f -> pure (Function f)
          Nothing -> pfail
      -- Match an alias for a built-in modifier
      readModifierAlias = do
        alias <- getName
        case Map.lookup alias modifierAliases of
          Just m -> pure (Modifier m)
          Nothing -> pfail
      -- Match a special value like #N
      readSpecialValue = do
        special <- getSpecialValue
        case Map.lookup special specialValues of
          Just v -> pure (SpecialValue v)
          Nothing -> pfail

-- To read a TokenList, skip leading whitespace and then either:
--  Successfully read a Token and continue parsing recursively
--  Hit a bad token and return an error
--  Hit the end of input and return success
instance Read TokenList where
  readPrec = do
    skipWhitespace
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
          restOfLine <- getRestOfLine
          skipWholeString
          pure $ TokenList $ Left $ case badToken of
            ('"' : _) -> "Malformed string literal: " ++ badToken ++ restOfLine
            ('\'' : _) -> "Malformed character literal: " ++ badToken
            ('`' : _) -> "Malformed interpolation string literal: " ++ badToken ++ restOfLine
            ('[' : _) -> "Malformed list literal: " ++ badToken ++ restOfLine
            _ -> "Unrecognized token: " ++ badToken
        -- If we are at the end of input, return a successful empty token list
        endOfInput = eof >> pure (TokenList $ Right [])

-- Parse a list of Tokens into a list of lists of Commands
-- For now, just put all the tokens in a single function
-- TODO: more-complex program structures?
parseTokens :: [Token] -> Either String [[Command]]
parseTokens tokens = Right [mapMaybe tokenToCommand tokens] where
  tokenToCommand (Function f) = Just $ PushFn f
  tokenToCommand (Interpolation i) = Just $ PushInterpolation i
  tokenToCommand (Modifier m) = Just $ ModifyFn m
  tokenToCommand (StackOp o) = Just $ StackCmd o
  tokenToCommand (Argument a) = Just $ BindArg a
  tokenToCommand (Literal v) = Just $ BindVal v
  tokenToCommand (SpecialValue v) = Just $ BindVal v
  tokenToCommand (Comment _) = Nothing

-- Scan a full program as a list of Tokens by reading a TokenList and
-- then extracting either a list of tokens or error message from it
scanProgram :: String -> Either String [Token]
scanProgram code = case readEither code of
  Right tokenList -> tokensOrError tokenList
  -- This case shouldn't happen, but handle it gracefully
  Left errorMessage -> Left $ "Error while scanning program: " ++ errorMessage

-- Parse a full program as a list of lists of Commands
parseProgram :: String -> Either String [[Command]]
parseProgram = (parseTokens =<<) . scanProgram

-- Parse an argument as either a Value or an error message
parseArg :: String -> Either String Value
parseArg arg = case readMaybe arg of
  Just x -> Right x
  Nothing -> Left $ "Could not parse argument " ++ arg

-- Parse a list of arguments as a list of Values or an error message
parseArgs :: [String] -> Either String [Value]
parseArgs = mapM parseArg
