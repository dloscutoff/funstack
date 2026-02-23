module Value (
  Value (..),
  ScalarValue (..),
  DisplayValue (..),
  fromValue,
  toValue,
  ord',
  chr',
  valToString,
  scalarToInteger,
  listOrString,
  sameTypeFalsey,
  depth,
  uniformDepth,
  flattenOnce,
  flattenAll,
  toIntegerList
) where

import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec (ReadPrec, choice, (<++))

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

-- To show a Value, convert it to a DisplayValue (see below) and show that
instance Show Value where
  show v = show (fromValue v :: DisplayValue)

-- To read a Value, read a DisplayValue and convert it to a Value
instance Read Value where
  readPrec = toValue <$> (readPrec :: ReadPrec DisplayValue)

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

-- Typeclass for converting from Value to another type
class FromValue a where
  fromValue :: Value -> a

instance FromValue Value where
  fromValue = id

instance FromValue Bool where
  -- Convert a Value to a Bool
  -- Falsey: 0, '\0', and empty list/string
  -- Truthy: everything else
  fromValue (ValNumber 0) = False
  fromValue (ValChar '\0') = False
  fromValue (ValList []) = False
  fromValue _ = True

instance (FromValue a) => FromValue [a] where
  -- Convert from a Value to a list of any FromValue type
  -- Given a ValList, return it as a Haskell list; given any other Value,
  -- wrap it in a singleton list
  fromValue (ValList l) = map fromValue l
  fromValue x = [fromValue x]

-- Typeclass for converting from another type to Value
class ToValue a where
  toValue :: a -> Value

instance ToValue Value where
  toValue = id

instance ToValue Char where
  toValue = ValChar

instance ToValue Bool where
  toValue True = ValNumber 1
  toValue False = ValNumber 0

instance ToValue Ordering where
  -- Convert LT, EQ, GT to ValNumber -1, ValNumber 0, or ValNumber 1, respectively
  toValue LT = ValNumber (-1)
  toValue EQ = ValNumber 0
  toValue GT = ValNumber 1

instance (ToValue a) => ToValue [a] where
  -- Convert from a list of any ToValue type to a Value
  toValue l = ValList $ map toValue l

-- ScalarValue is a helper type that does not include lists
data ScalarValue =
  ScalarNumber Integer |
  ScalarChar Char

instance ToValue ScalarValue where
  toValue (ScalarNumber n) = ValNumber n
  toValue (ScalarChar c) = ValChar c

-- DisplayValue is a helper type that distinguishes between strings (lists
-- of characters) and other lists to facilitate displaying them differently
data DisplayValue =
  AsNumber Integer |
  AsChar Char |
  AsString String |
  AsList [Value]

instance FromValue DisplayValue where
  -- ValNumber and ValChar correspond directly to AsNumber and AsChar;
  -- a ValList could be either AsList or AsString
  --  ValLists are considered to be strings if they are nonempty and
  --   contain only ValChars; otherwise, they are considered to be lists
  --  In practice, it is not possible to check the entire list because it could
  --   be infinite, so we check the first 1,000 elements
  --  If a supposed string turns out to contain non-ValChars after the first
  --   1,000 elements, the non-ValChars are replaced with null characters
  fromValue (ValNumber n) = AsNumber n
  fromValue (ValChar c) = AsChar c
  fromValue (ValList []) = AsList []
  fromValue (ValList l)
    | isProbablyString l = AsString $ map valToChar l
    | otherwise = AsList l
    where
      isProbablyString = and . map isCharacter . take 1000
      isCharacter v
        | (ValChar _) <- v = True
        | otherwise = False
      valToChar v
        | (ValChar c) <- v = c
        | otherwise = '\0'

instance ToValue DisplayValue where
  toValue (AsNumber n) = ValNumber n
  toValue (AsChar c) = ValChar c
  toValue (AsList l) = ValList l
  toValue (AsString s) = toValue s

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

-- Convert a Value to a String
-- This function behaves differently from show: it does not wrap strings
-- and characters in quotes or escape any characters in them
valToString :: Value -> String
valToString = toString . fromValue
  where
    toString (AsChar c) = [c]
    toString (AsString s) = s
    toString x = show x

-- Convert a ScalarValue to an Integer
--  If it is a ScalarNumber, return the corresponding Integer
--  If it is a ScalarChar, return its codepoint
scalarToInteger :: ScalarValue -> Integer
scalarToInteger (ScalarNumber n) = n
scalarToInteger (ScalarChar c) = ord' c

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
flattenOnce = concatMap fromValue

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
toIntegerList = map scalarToInteger . flattenAll . fromValue
