module Value (
  Value (..),
  ScalarValue (..),
  DisplayValue (..),
  ord',
  chr',
  isProbablyString,
  valToDisplay,
  valToString,
  valToBool,
  valToList,
  scalarToInteger,
  scalarToVal,
  stringToVal,
  boolToVal,
  orderingToVal,
  listOrSingleton,
  listOrString,
  sameTypeFalsey,
  depth,
  uniformDepth,
  flattenOnce,
  flattenAll,
  toIntegerList
) where

import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec (choice, (<++))

-- Value is the main data type for values in FunStack:
--  ValNumber represents an arbitrary-size integer
--  ValChar represents a single Unicode character
--  ValList represents a list containing other Values (potentially of
--   different types, including other ValLists)
data Value =
  ValNumber Integer |
  ValChar Char |
  ValList [Value]
  deriving (Eq)

-- ScalarValue is a helper type that does not include lists
data ScalarValue =
  ScalarNumber Integer |
  ScalarChar Char

-- DisplayValue is a helper type that distinguishes between strings (lists
-- of characters) and other lists to facilitate displaying them differently
data DisplayValue =
  AsNumber Integer |
  AsChar Char |
  AsString String |
  AsList [Value]

instance Show DisplayValue where
  show (AsNumber n) = show n
  show (AsChar c) = show c
  show (AsString s) = show s
  show (AsList l) = show l

instance Read DisplayValue where
  readPrec = choice [
    AsNumber <$> readPrec,
    AsChar <$> readPrec,
    (AsList <$> readPrec) <++ (AsString <$> readPrec)
    ]

-- Given a Char, return its codepoint as an Integer
ord' :: Char -> Integer
ord' = toInteger . fromEnum

-- Given an Integer, return the corresponding Char
-- If the Integer is out of bounds, reduce it modulo the maximum Unicode
-- codepoint plus 1
chr' :: Integer -> Char
chr' = toEnum . fromInteger . (`mod` totalChars)
  where totalChars = toInteger $ 1 + fromEnum (maxBound :: Char)

-- Given a list of Values, are all of them ValChars?
-- In practice, it is not possible to check the entire list because it could
-- be infinite, so we check the first 10,000 elements
isProbablyString :: [Value] -> Bool
isProbablyString = and . map isCharacter . take 10000
  where
    isCharacter (ValChar _) = True
    isCharacter _ = False

-- Convert a Value to a DisplayValue
--  ValLists are considered to be strings if they are nonempty and
--   isProbablyString returns True
--  If a supposed string turns out to contain non-ValChars after the first
--   10,000 elements, the non-ValChars are replaced with null characters
valToDisplay :: Value -> DisplayValue
valToDisplay (ValNumber n) = AsNumber n
valToDisplay (ValChar c) = AsChar c
valToDisplay (ValList []) = AsList []
valToDisplay (ValList l)
  | isProbablyString l = AsString $ map valToChar l
  | otherwise = AsList l
  where
    valToChar (ValChar c) = c
    valToChar _ = '\0'

-- Convert a DisplayValue to a Value
displayToVal :: DisplayValue -> Value
displayToVal (AsNumber n) = ValNumber n
displayToVal (AsChar c) = ValChar c
displayToVal (AsList l) = ValList l
displayToVal (AsString s) = stringToVal s

-- To show a Value, convert it to a DisplayValue and show that
instance Show Value where
  show = show . valToDisplay

-- To read a Value, read a DisplayValue and convert it
instance Read Value where
  readPrec = displayToVal <$> readPrec

-- Values exist in a total ordering:
--  ValChars are less than ValNumbers
--  ValNumbers are less than ValLists
--  If both values have the same type, compare their contents
instance Ord Value where
  (ValChar c) `compare` (ValChar d) = c `compare` d
  (ValChar _) `compare` _ = LT
  _ `compare` (ValChar _) = GT
  (ValNumber x) `compare` (ValNumber y) = x `compare` y
  (ValNumber _) `compare` _ = LT
  _ `compare` (ValNumber _) = GT
  (ValList l) `compare` (ValList m) = l `compare` m

-- Convert a Value to a String
-- This function behaves differently from show: it does not wrap strings
-- and characters in quotes or escape any characters in them
valToString :: Value -> String
valToString = toString . valToDisplay
  where
    toString (AsChar c) = [c]
    toString (AsString s) = s
    toString x = show x

-- Convert a Value to a Bool
-- Falsey: 0, '\0', and empty list/string
-- Truthy: everything else
valToBool :: Value -> Bool
valToBool (ValNumber 0) = False
valToBool (ValChar '\0') = False
valToBool (ValList []) = False
valToBool _ = True

-- If the Value is a ValList, return the corresponding Haskell list
valToList :: Value -> Maybe [Value]
valToList (ValList l) = Just l
valToList _ = Nothing

-- Convert a ScalarValue to an Integer
--  If it is a ScalarNumber, return the corresponding Integer
--  If it is a ScalarChar, return its codepoint
scalarToInteger :: ScalarValue -> Integer
scalarToInteger (ScalarNumber n) = n
scalarToInteger (ScalarChar c) = ord' c

-- Convert a ScalarValue to a Value of the same type
scalarToVal :: ScalarValue -> Value
scalarToVal (ScalarNumber n) = ValNumber n
scalarToVal (ScalarChar c) = ValChar c

-- Convert a Haskell string to a ValList of ValChars
stringToVal :: String -> Value
stringToVal = ValList . map ValChar

-- Convert True or False to ValNumber 1 or ValNumber 0, respectively
boolToVal :: Bool -> Value
boolToVal = ValNumber . toInteger . fromEnum

-- Convert LT, EQ, GT to ValNumber -1, ValNumber 0, or ValNumber 1, respectively
-- By default, fromEnum converts LT, EQ, GT to 0, 1, 2, so we compose it
-- with pred to get -1, 0, 1
orderingToVal :: Ordering -> Value
orderingToVal = ValNumber . toInteger . pred . fromEnum

-- Given a ValList, return it as a [Value]; given any other Value, wrap it in
-- a singleton list
listOrSingleton :: Value -> [Value]
listOrSingleton (ValList l) = l
listOrSingleton x = [x]

-- Given a ValList, return it as a [Value]; given any other Value, stringify
-- it and return it as a list of ValChars
listOrString :: Value -> [Value]
listOrString (ValList l) = l
listOrString x = map ValChar $ valToString x

-- Given a Value, return a falsey Value of the same type
sameTypeFalsey :: Value -> Value
sameTypeFalsey (ValNumber _) = ValNumber 0
sameTypeFalsey (ValChar _) = ValChar '\0'
sameTypeFalsey (ValList _) = ValList []

-- Takes a Value and returns its depth:
--  Depth of a scalar is 0
--  Depth of an empty ValList is 1
--  Depth of a nonempty ValList is 1 plus the depth of its deepest element
depth :: Value -> Integer
depth (ValNumber _) = 0
depth (ValChar _) = 0
depth (ValList []) = 1
depth (ValList l) = 1 + maximum (map depth l)

-- Takes a Value that is assumed to be either a scalar or a uniformly
-- deeply nested ValList and returns its depth
-- Unlike depth, this function does not get into an infinite loop when given
-- an infinite ValList
--  Uniform depth of a nonempty ValList is 1 plus the depth of its first
--   element
--  Uniform depth of an empty ValList or a scalar is the same as its
--   regular depth
uniformDepth :: Value -> Integer
uniformDepth (ValList (r : _)) = 1 + uniformDepth r
uniformDepth x = depth x

-- Take a list of Values; convert ValLists to Haskell lists and scalars to
-- singleton lists, concatenate them all together, and return that list
flattenOnce :: [Value] -> [Value]
flattenOnce = concatMap listOrSingleton

-- Take a list of Values that may include ValLists; return a flat list of
-- ScalarValues
flattenAll :: [Value] -> [ScalarValue]
flattenAll [] = []
flattenAll (ValNumber n : xs) = ScalarNumber n : flattenAll xs
flattenAll (ValChar c : xs) = ScalarChar c : flattenAll xs
flattenAll (ValList l : xs) = flattenAll l ++ flattenAll xs

-- Take a Value and convert it to a list of Integers:
--  If it's a scalar, wrap it in a singleton list
--  Flatten the list
--  Convert any characters in the result to their codepoints
toIntegerList :: Value -> [Integer]
toIntegerList = map scalarToInteger . flattenAll . listOrSingleton
