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
--  Number represents an arbitrary-size integer
--  Character represents a single Unicode character
--  List represents a list containing other Values (potentially of
--   different types, including other Lists)
data Value = Number Integer | Character Char | List [Value]

-- ScalarValue is a helper type that does not include lists
data ScalarValue = ScalarNumber Integer | ScalarChar Char

-- DisplayValue is a helper type that distinguishes between strings (Lists
-- of Characters) and other Lists to facilitate displaying them differently
data DisplayValue = AsNumber Integer | AsChar Char | AsString String | AsList [Value]

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

-- Given a list of Values, are all of them Characters?
-- In practice, it is not possible to check the entire list because it could
-- be infinite, so we check the first 10,000 elements
isProbablyString :: [Value] -> Bool
isProbablyString = and . map isCharacter . take 10000
  where
    isCharacter (Character _) = True
    isCharacter _ = False

-- Convert a Value to a DisplayValue
--  Lists are considered to be strings if they are nonempty and
--   isProbablyString returns True
--  If a supposed string turns out to contain non-Characters after the first
--   10,000 elements, the non-Characters are replaced with null characters
valToDisplay :: Value -> DisplayValue
valToDisplay (Number n) = AsNumber n
valToDisplay (Character c) = AsChar c
valToDisplay (List []) = AsList []
valToDisplay (List l)
  | isProbablyString l = AsString $ map valToChar l
  | otherwise = AsList l
  where
    valToChar (Character c) = c
    valToChar _ = '\0'

-- Convert a DisplayValue to a Value
displayToVal :: DisplayValue -> Value
displayToVal (AsNumber n) = Number n
displayToVal (AsChar c) = Character c
displayToVal (AsList l) = List l
displayToVal (AsString s) = stringToVal s

-- To show a Value, convert it to a DisplayValue and show that
instance Show Value where
  show = show . valToDisplay

-- To read a Value, read a DisplayValue and convert it
instance Read Value where
  readPrec = displayToVal <$> readPrec

-- Two Values are equal if they are the same type and their contents are equal
instance Eq Value where
  (Number x) == (Number y) = x == y
  (Character c) == (Character d) = c == d
  (List l) == (List m) = l == m
  _ == _ = False

-- Values exist in a total ordering:
--  Characters are less than Numbers
--  Numbers are less than Lists
--  If both values have the same type, compare their contents
instance Ord Value where
  (Character c) `compare` (Character d) = c `compare` d
  (Character _) `compare` _ = LT
  _ `compare` (Character _) = GT
  (Number x) `compare` (Number y) = x `compare` y
  (Number _) `compare` _ = LT
  _ `compare` (Number _) = GT
  (List l) `compare` (List m) = l `compare` m

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
valToBool (Number 0) = False
valToBool (Character '\0') = False
valToBool (List []) = False
valToBool _ = True

-- If the Value is a List, return the corresponding Haskell list
valToList :: Value -> Maybe [Value]
valToList (List l) = Just l
valToList _ = Nothing

-- Convert a ScalarValue to an Integer
--  If it is a Number, return the corresponding Integer
--  If it is a Character, return its codepoint
scalarToInteger :: ScalarValue -> Integer
scalarToInteger (ScalarNumber x) = x
scalarToInteger (ScalarChar c) = ord' c

-- Convert a ScalarValue to a Value of the same type
scalarToVal :: ScalarValue -> Value
scalarToVal (ScalarNumber x) = Number x
scalarToVal (ScalarChar c) = Character c

-- Convert a Haskell string to a List of Characters
stringToVal :: String -> Value
stringToVal = List . map Character

-- Convert True or False to Number 1 or Number 0, respectively
boolToVal :: Bool -> Value
boolToVal = Number . toInteger . fromEnum

-- Convert LT, EQ, GT to Number -1, Number 0, or Number 1, respectively
-- By default, fromEnum converts LT, EQ, GT to 0, 1, 2, so we compose it
-- with pred to get -1, 0, 1
orderingToVal :: Ordering -> Value
orderingToVal = Number . toInteger . pred . fromEnum

-- Given a List, return it as a [Value]; given any other Value, wrap it in
-- a singleton list
listOrSingleton :: Value -> [Value]
listOrSingleton (List l) = l
listOrSingleton x = [x]

-- Given a List, return it as a [Value]; given any other Value, stringify
-- it and return it as a list of Characters
listOrString :: Value -> [Value]
listOrString (List l) = l
listOrString x = map Character $ valToString x

-- Given a Value, return a falsey Value of the same type
sameTypeFalsey :: Value -> Value
sameTypeFalsey (Number _) = Number 0
sameTypeFalsey (Character _) = Character '\0'
sameTypeFalsey (List _) = List []

-- Takes a Value and returns its depth:
--  Depth of a scalar is 0
--  Depth of an empty List is 1
--  Depth of a nonempty List is 1 plus the depth of its deepest element
depth :: Value -> Integer
depth (Number _) = 0
depth (Character _) = 0
depth (List []) = 1
depth (List l) = 1 + maximum (map depth l)

-- Takes a Value that is assumed to be either a scalar or a uniformly
-- deeply nested List and returns its depth
-- Unlike depth, this function does not get into an infinite loop when given
-- an infinite List
--  Uniform depth of a nonempty List is 1 plus the depth of its first
--   element
--  Uniform depth of an empty List or a scalar is the same as its
--   regular depth
uniformDepth :: Value -> Integer
uniformDepth (List (r : _)) = 1 + uniformDepth r
uniformDepth x = depth x

-- Take a list of Values; convert Lists to Haskell lists and scalars to
-- singleton lists, concatenate them all together, and return that list
flattenOnce :: [Value] -> [Value]
flattenOnce = concatMap listOrSingleton

-- Take a list of Values that may include Lists; return a flat list of
-- ScalarValues
flattenAll :: [Value] -> [ScalarValue]
flattenAll [] = []
flattenAll (Number x : vs) = ScalarNumber x : flattenAll vs
flattenAll (Character c : vs) = ScalarChar c : flattenAll vs
flattenAll (List l : vs) = flattenAll l ++ flattenAll vs

-- Take a Value and convert it to a list of Integers:
--  If it's a scalar, wrap it in a singleton list
--  Flatten the list
--  Convert any characters in the result to their codepoints
toIntegerList :: Value -> [Integer]
toIntegerList = map scalarToInteger . flattenAll . listOrSingleton
