module Value (
  Value (..),
  ScalarValue (..),
  DisplayValue (..),
  FromValue,
  fromValue,
  ToValue,
  toValue,
  ord',
  chr',
  valToString,
  scalarToNumber,
  listOrString,
  sameTypeFalsey,
  depth,
  uniformDepth,
  flattenOnce,
  flattenAll,
  toNumberList
) where

import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec (ReadPrec, choice, (<++))
import Number (Number, toNumber)

-- Value is the main data type for values in FunStack:
--  List represents a list containing other Values (potentially of
--   different types, including other Lists)
--  Scalar represents a non-list (number or character; see ScalarValue
--   for details)
data Value =
  List [Value] |
  Scalar ScalarValue
  deriving (Eq)

-- To show a Value, convert it to a DisplayValue (see below) and show that
instance Show Value where
  show v = show (fromValue v :: DisplayValue)

-- To read a Value, read a DisplayValue and convert it to a Value
instance Read Value where
  readPrec = toValue <$> (readPrec :: ReadPrec DisplayValue)

-- Values exist in a total ordering:
--  Scalars are less than Lists
--  If both values have the same type, compare their contents
instance Ord Value where
  (Scalar x) `compare` (Scalar y) = x `compare` y
  (Scalar _) `compare` (List _) = LT
  (List _) `compare` (Scalar _) = GT
  (List l) `compare` (List m) = l `compare` m

-- Typeclass for converting from Value to another type
class FromValue a where
  fromValue :: Value -> a

instance FromValue Value where
  fromValue = id

instance FromValue Bool where
  -- Convert a Value to a Bool
  -- Falsey: 0, '\0', and empty list/string
  -- Truthy: everything else
  fromValue (Scalar (ScalarNumber 0)) = False
  fromValue (Scalar (ScalarChar '\0')) = False
  fromValue (List []) = False
  fromValue _ = True

instance (FromValue a) => FromValue [a] where
  -- Convert from a Value to a list of any FromValue type
  -- Given a List, return it as a Haskell list; given any other Value,
  -- wrap it in a singleton list
  fromValue (List l) = map fromValue l
  fromValue x = [fromValue x]

-- Typeclass for converting from another type to Value
class ToValue a where
  toValue :: a -> Value

instance ToValue Value where
  toValue = id

instance ToValue Number where
  toValue = Scalar . ScalarNumber

instance ToValue Char where
  toValue = Scalar . ScalarChar

instance ToValue Bool where
  -- Convert True and False to 1 and 0, respectively
  toValue True = Scalar $ ScalarNumber 1
  toValue False = Scalar $ ScalarNumber 0

instance ToValue Ordering where
  -- Convert LT, EQ, GT to -1, 0, or 1, respectively
  toValue LT = Scalar $ ScalarNumber (-1)
  toValue EQ = Scalar $ ScalarNumber 0
  toValue GT = Scalar $ ScalarNumber 1

instance (ToValue a) => ToValue [a] where
  -- Convert from a list of any ToValue type to a Value
  toValue l = List $ map toValue l

-- ScalarValue is a helper type that does not include lists
data ScalarValue =
  ScalarNumber Number |
  ScalarChar Char
  deriving (Eq)

-- ScalarValues exist in a total ordering:
--  ScalarChars are less than ScalarNumbers
--  If both values have the same type, compare their contents
instance Ord ScalarValue where
  (ScalarChar c) `compare` (ScalarChar d) = c `compare` d
  (ScalarChar _) `compare` (ScalarNumber _) = LT
  (ScalarNumber _) `compare` (ScalarChar _) = GT
  (ScalarNumber x) `compare` (ScalarNumber y) = x `compare` y

instance ToValue ScalarValue where
  toValue = Scalar

-- DisplayValue is a helper type that distinguishes between strings (lists
-- of characters) and other lists to facilitate displaying them differently
data DisplayValue =
  AsNumber Number |
  AsChar Char |
  AsString String |
  AsList [Value]

instance FromValue DisplayValue where
  -- ScalarNumber and ScalarChar correspond directly to AsNumber and AsChar;
  -- a List could be either AsList or AsString
  --  Lists are considered to be strings if they are nonempty and
  --   contain only ScalarChars; otherwise, they are considered to be lists
  --  In practice, it is not possible to check the entire list because it could
  --   be infinite, so we check the first 1,000 elements
  --  If a supposed string turns out to contain non-characters after the first
  --   1,000 elements, the non-characters are replaced with null characters
  fromValue (Scalar (ScalarNumber n)) = AsNumber n
  fromValue (Scalar (ScalarChar c)) = AsChar c
  fromValue (List []) = AsList []
  fromValue (List l)
    | isProbablyString l = AsString $ map valToChar l
    | otherwise = AsList l
    where
      isProbablyString = and . map isCharacter . take 1000
      isCharacter v
        | (Scalar (ScalarChar _)) <- v = True
        | otherwise = False
      valToChar v
        | (Scalar (ScalarChar c)) <- v = c
        | otherwise = '\0'

instance ToValue DisplayValue where
  toValue (AsNumber n) = Scalar $ ScalarNumber n
  toValue (AsChar c) = Scalar $ ScalarChar c
  toValue (AsList l) = List l
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

-- Given a Char, return its codepoint as a Number
ord' :: Char -> Number
ord' = toNumber . fromEnum

-- Given a Number, return the corresponding Char
-- If the Number is out of bounds, reduce it modulo the maximum Unicode
-- codepoint plus 1
chr' :: Number -> Char
chr' = toEnum . fromInteger . (`mod` totalChars) . toInteger
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

-- Convert a ScalarValue to a Number
--  If it is a ScalarNumber, return the corresponding Number
--  If it is a ScalarChar, return its codepoint
scalarToNumber :: ScalarValue -> Number
scalarToNumber (ScalarNumber n) = n
scalarToNumber (ScalarChar c) = ord' c

-- Given a List, return it as a [Value]; given any other Value, stringify
-- it and return its characters as a list of Scalars
listOrString :: Value -> [Value]
listOrString (List l) = l
listOrString x = map toValue $ valToString x

-- Given a Value, return a falsey Value of the same type
sameTypeFalsey :: Value -> Value
sameTypeFalsey (Scalar (ScalarNumber _)) = Scalar $ ScalarNumber 0
sameTypeFalsey (Scalar (ScalarChar _)) = Scalar $ ScalarChar '\0'
sameTypeFalsey (List _) = List []

-- Takes a Value and returns its depth:
--  Depth of a Scalar is 0
--  Depth of an empty List is 1
--  Depth of a nonempty List is 1 plus the depth of its deepest element
depth :: Value -> Number
depth (Scalar _) = 0
depth (List []) = 1
depth (List l) = 1 + maximum (map depth l)

-- Takes a Value that is assumed to be either a Scalar or a uniformly
-- deeply nested List and returns its depth
-- Unlike depth, this function does not get into an infinite loop when given
-- an infinite List
--  Uniform depth of a nonempty List is 1 plus the depth of its first
--   element
--  Uniform depth of an empty List or a Scalar is the same as its
--   regular depth
uniformDepth :: Value -> Number
uniformDepth (List (r : _)) = 1 + uniformDepth r
uniformDepth x = depth x

-- Take a list of Values; convert Lists to Haskell lists and Scalars to
-- singleton lists, concatenate them all together, and return that list
flattenOnce :: [Value] -> [Value]
flattenOnce = concatMap fromValue

-- Take a list of Values that may include Lists; return a flat list of
-- ScalarValues
flattenAll :: [Value] -> [ScalarValue]
flattenAll [] = []
flattenAll (Scalar x : xs) = x : flattenAll xs
flattenAll (List l : xs) = flattenAll l ++ flattenAll xs

-- Take a Value and convert it to a list of Numbers:
--  If it's a Scalar, wrap it in a singleton list
--  Flatten the list
--  Convert any characters in the result to their codepoints
toNumberList :: Value -> [Number]
toNumberList = map scalarToNumber . flattenAll . fromValue
