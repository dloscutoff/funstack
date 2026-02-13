module CondensedParser (
  parseProgram
) where

import Data.Char (isSpace, isDigit)
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
import Value (Value (..))
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
  SpecialValue Value
  deriving (Show)

data TokenList = TokenList {
  tokensOrError :: Either String [Token]
}

-- Condensed names for built-in functions
functionAliases :: [(String, BF.BuiltinFunction)]
functionAliases = [
  ("At", BF.At),
  ("Cc", BF.Concat),
  ("Co", BF.Const),
  ("Cr", BF.Consr),
  ("Cy", BF.Cycle),
  ("Dc", BF.Dec),
  ("Di", BF.IDiv),
  ("Dp", BF.Depth),
  ("Dr", BF.Drop),
  ("Eq", BF.Equal),
  ("F0", BF.From0),
  ("Fb", BF.FromBase),
  ("Fl", BF.Flatten),
  ("Hd", BF.Head),
  ("I1", BF.IFrom1),
  ("Ic", BF.Inc),
  ("In", BF.Init),
  ("La", BF.Last),
  ("Lg", BF.Length),
  ("Ln", BF.Lines),
  ("Ls", BF.Less),
  ("Mi", BF.Minus),
  ("Mo", BF.Mod),
  ("Ng", BF.Neg),
  ("Pa", BF.Pair),
  ("Pw", BF.Pow),
  ("Rp", BF.Repeat),
  ("Rv", BF.Reverse),
  ("Sh", BF.Show),
  ("Su", BF.Sum),
  ("Ta", BF.Take),
  ("Tb", BF.ToBase),
  ("Ti", BF.Times),
  ("Tl", BF.Tail),
  ("Wr", BF.Wrap),
  ("C", BF.Cons),
  ("P", BF.Plus),
  ("I", BF.Id)
  ]

-- Condensed names for built-in modifiers
modifierAliases :: [(String, BM.BuiltinModifier)]
modifierAliases = [
  ("?&", BM.And),
  ("/\\", BM.Branch),
  ("(?", BM.Dropwhile),
  ("{+", BM.Foldr),
  ("{{", BM.Foldr1),
  ("<>", BM.Fork),
  ("(>", BM.Hook),
  ("??", BM.If),
  ("::", BM.Iterate),
  ("?!", BM.Not),
  ("?|", BM.Or),
  ("+]", BM.Scanl),
  ("]]", BM.Scanl1),
  ("()", BM.Self),
  ("%%", BM.Table),
  (")?", BM.Takewhile),
  (":%", BM.Unfoldr),
  (":?", BM.While),
  (".", BM.Compose),
  ("~", BM.Flip),
  ("*", BM.Mapzip)
  ]

-- Condensed names for built-in stack operators
stackOpAliases :: [(String, BSO.BuiltinStackOp)]
stackOpAliases = [
  ("!ub", BSO.Unbox),
  ("!b", BSO.Box),
  ("!d", BSO.Dup),
  ("!o", BSO.Over),
  ("!r", BSO.Rot),
  ("!s", BSO.Swap),
  ("!t", BSO.Tuck)
  ]

-- Constants that aren't number/character/string/list literals
specialValues :: [(String, Value)]
specialValues = [
  ("#N1", List $ map Number [1..]),
  ("\\n", Character '\n'),
  ("\\s", Character ' '),
  ("$A", List $ map Character ['A'..'Z']),
  ("$a", List $ map Character ['a'..'z']),
  ("$0", List $ map Character ['0'..'9']),
  ("#-", Number (-1)),
  ("#t", Number 10),
  ("#N", List $ map Number [0..])
  ]

-- Helper ReadPrec parsers for the Read instances below:

-- Skip over zero or more non-newline whitespace characters
skipSpaces :: ReadPrec ()
skipSpaces = lift $ ReadP.munch (\c -> isSpace c && c /= '\n') >> pure ()

-- Match a single digit
getDigit :: ReadPrec Char
getDigit = lift $ ReadP.satisfy isDigit

-- Helper ReadP parser
-- Match a run of digits, possibly with a leading minus sign
getNumber' :: ReadP.ReadP String
getNumber' = do
  sign <- ReadP.option "" (ReadP.string "-")
  digits <- ReadP.munch1 isDigit
  pure $ sign ++ digits

-- Match exactly one number
getNumber :: ReadPrec String
getNumber = lift getNumber'

-- Match zero or more numbers, separated by commas
getNumbers :: ReadPrec [String]
getNumbers = lift $ ReadP.sepBy getNumber' (ReadP.char ',')

-- Match non-space printable ASCII character
getNonSpaceChar :: ReadPrec Char
getNonSpaceChar = lift $ ReadP.choice $ map ReadP.char ['!'..'~']

-- Helper ReadP parser
-- Match either a half-byte or a full-byte character within a string literal
-- Full-byte printable ASCII characters are preceded by '
-- Newline is a full-byte character also, encoded as nl
getStringCharacter' :: ReadP.ReadP Char
getStringCharacter' = ReadP.choice [
  ReadP.choice (map ReadP.char halfByteCharacters),
  ReadP.char '\'' >> ReadP.choice (map ReadP.char fullByteCharacters),
  ReadP.string "~n" >> pure '\n'
  ]
  where
    halfByteCharacters = " /-\\|.+_"
    fullByteCharacters = [c | c <- ['!'..'~'], c `notElem` halfByteCharacters]

-- Match the contents of a string literal, possibly with interpolations
getInterpolationComponents :: ReadPrec [I.InterpolationComponent]
getInterpolationComponents = lift $ ReadP.many $ ReadP.choice [
  fmap (I.LiteralString . pure) getStringCharacter',
  ReadP.string "$" >> pure I.Interpolate
  ]

-- Match the rest of the string
getRestOfString :: ReadPrec String
getRestOfString = lift $ ReadP.munch1 (const True)

-- Match any of the first elements of the tuples in a lookup list
-- and return the corresponding second element
-- If there are any lookup strings that are prefixes of other
-- lookup strings, the longer string must come earlier in the list
getLookup :: [(String, a)] -> ReadPrec a
getLookup l = lift $ foldr (ReadP.<++) ReadP.pfail [ReadP.string s >> pure v | (s, v) <- l]

-- Match the end of input
eof :: ReadPrec ()
eof = lift $ ReadP.eof

-- TODO: in progress
-- To read a Token, read a built-in function, modifier, or stack operation;
-- an argument reference; a literal; a function or modifier alias; or a
-- special value
instance Read Token where
  readPrec = choice [
    readFunctionAlias,
    readModifierAlias,
    readStackOpAlias,
    readSpecialValue,
    readDigitLiteral,
    readCharLiteral,
    readNumberLiteral <++ readNumberListLiteral,
    readInterpolation,
    readArgReference
    ] where
      -- Match an alias for a built-in function
      readFunctionAlias = Function <$> getLookup functionAliases
      -- Match an alias for a built-in modifier
      readModifierAlias = Modifier <$> getLookup modifierAliases
      -- Match an alias for a built-in stack operator
      readStackOpAlias = StackOp <$> getLookup stackOpAliases
      -- Match a special value like #N
      readSpecialValue = SpecialValue <$> getLookup specialValues
      -- Match a digit literal like #1
      readDigitLiteral = do
        '#' <- get
        digit <- getDigit
        case readMaybe [digit] of
          Just n -> pure (Literal $ Number n)
          Nothing -> pfail
      -- Match a character literal like ''x'
      readCharLiteral = do
        '\'' <- get
        '\'' <- get
        char <- getNonSpaceChar
        '\'' <- get
        pure (Literal $ Character char)
      -- Match a numeric literal like #"-123"
      readNumberLiteral = do
        '#' <- get
        '"' <- get
        number <- getNumber
        '"' <- get
        case readMaybe number of
          Just n -> pure (Literal $ Number n)
          Nothing -> pfail
      -- Match a numeric list literal like #"-12,345"
      readNumberListLiteral = do
        '#' <- get
        '"' <- get
        numbers <- getNumbers
        '"' <- get
        case sequence (map readMaybe numbers) of
          Just ns -> pure (Literal $ List $ map Number ns)
          Nothing -> pfail
      -- Match an interpolation/string literal like $"'a'b-$'c"
      readInterpolation = do
        '$' <- get
        '"' <- get
        components <- getInterpolationComponents
        '"' <- get
        pure (Interpolation $ I.condense components)
      -- Match an argument reference like @1
      readArgReference = do
        '@' <- get
        argNumber <- getDigit
        case readMaybe [argNumber] of
          Just n -> pure (Argument n)
          Nothing -> pfail

-- To read a TokenList, either:
--  Successfully read a Token and continue parsing recursively
--  Hit a bad token and return an error
--  Hit the end of input and return success
instance Read TokenList where
  readPrec = do
    skipSpaces
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
          restOfString <- getRestOfString
          pure $ TokenList $ Left $ "Unrecognized token starting at: " ++ take 20 restOfString
        -- If we are at the end of input, return a successful empty token list
        endOfInput = eof >> pure (TokenList $ Right [])

-- Parse a list of Tokens into a list of lists of Commands
-- For now, just put all the tokens in a single function
parseTokens :: [Token] -> Either String [[Command]]
parseTokens tokens = Right [mapMaybe tokenToCommand tokens] where
  tokenToCommand (Function f) = Just $ PushFn f
  tokenToCommand (Interpolation i) = Just $ PushInterpolation i
  tokenToCommand (Modifier m) = Just $ ModifyFn m
  tokenToCommand (StackOp o) = Just $ StackCmd o
  tokenToCommand (Argument a) = Just $ BindArg a
  tokenToCommand (Literal v) = Just $ BindVal v
  tokenToCommand (SpecialValue v) = Just $ BindVal v

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
