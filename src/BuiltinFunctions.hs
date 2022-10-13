module BuiltinFunctions (
  builtins
) where

import Data.List (
  subsequences,
  unfoldr,
  genericTake,
  genericDrop,
  genericIndex,
  genericReplicate
  )
import qualified Data.Map as Map
import Value (
  Value (..),
  ScalarValue (..),
  DisplayValue (..),
  chr',
  valToDisplay,
  valToBool,
  valToString,
  scalarToInteger,
  stringToVal,
  boolToVal,
  listOrSingleton,
  sameTypeFalsey
  )
import Function (
  Function,
  monadic,
  dyadic,
  triadic,
  tetradic,
  mapOverList,
  mapOverLists,
  charMathMonad,
  charMathDyad,
  numberMathMonad,
  numberMathDyad,
--  numToListMonad,
  numToListDyad,
  numAndListDyad
  )

-- Convert True to 1 and False to 0
boolToInteger :: Bool -> Integer
boolToInteger = toInteger . fromEnum

-- Given two ScalarValues, generate an inclusive range from the first to
-- the second, coercing to the type of the second argument
inclRange :: ScalarValue -> ScalarValue -> Value
inclRange x (ScalarNumber y) = List $ map Number [scalarToInteger x..y]
inclRange x (ScalarChar c) = List $ map Character [chr' n..c]
  where n = max 0 (scalarToInteger x)

-- Given two ScalarValues, generate an exclusive range from the first to
-- the second, coercing to the type of the second argument
exclRange :: ScalarValue -> ScalarValue -> Value
exclRange x (ScalarNumber y) = List $ map Number [scalarToInteger x..y-1]
exclRange x (ScalarChar c)
  | c == minBound = List []
  | otherwise = List $ map Character [chr' n..pred c]
  where n = max 0 (scalarToInteger x)

-- Take the first n elements from a list; if n is negative, take elements
-- from the end
take' :: Integer -> [a] -> [a]
take' n
  | n >= 0 = genericTake n
  | otherwise = reverse . genericTake (abs n) . reverse

-- Drop the first n elements from a list; if n is negative, drop elements
-- from the end
drop' :: Integer -> [a] -> [a]
drop' n
  | n >= 0 = genericDrop n
  | otherwise = reverse . genericDrop (abs n) . reverse

-- Take the first n elements from a list, repeating as necessary; if n is
-- negative, take elements starting from the end
takeCycle :: Integer -> [a] -> [a]
takeCycle n
  | n >= 0 = genericTake n . cycle
  | otherwise = reverse . genericTake (abs n) . cycle . reverse

-- Get the nth element in a list, repeating as necessary; if n is negative,
-- count backwards from the end
indexCycle :: [a] -> Integer -> a
indexCycle l n
  | n >= 0 = genericIndex (cycle l) n
  | otherwise = genericIndex (cycle $ reverse l) (abs n - 1)

-- Rotate a list n elements to the left (to the right if n is negative)
rotate :: Integer -> [a] -> [a]
rotate _ [] = []
rotate 0 l = l
rotate n l
  | minlength < abs n = rotate (n `mod` minlength) l
  | n > 0 = drop' n l ++ take' n l
  | otherwise = take' n l ++ drop' n l
  where minlength = toInteger (length (take' n l))

-- Get all length-n slices of a list
--  If n is negative, start from the end of the list
--  If n is zero, get all nonempty slices
windows :: Integer -> [a] -> [[a]]
windows n
  | n < 0 = reverse . windows (abs n)
  | n == 0 = tail . subsequences
  | otherwise = unfoldr (\l -> if null (drop' (n - 1) l)
                               then Nothing
                               else Just (take' n l, tail l))

-- Merge two lists into a single list, alternating their elements
-- Once one list runs out of elements, append the remainder of the other list
interleave :: [a] -> [a] -> [a]
interleave [] l = l
interleave (h : t) l = h : interleave l t

-- Takes a list of Values that may include Lists; returns a flat list of
-- ScalarValues
flatten :: [Value] -> [ScalarValue]
flatten [] = []
flatten (Number x : vs) = ScalarNumber x : flatten vs
flatten (Character c : vs) = ScalarChar c : flatten vs
flatten (List l : vs) = flatten l ++ flatten vs

-- Takes a Value and returns its depth:
--  Depth of a scalar is 0
--  Depth of an empty List is 0
--  Depth of a nonempty List is 1 plus the depth of its deepest element
depth :: Value -> Integer
depth (Number _) = 0
depth (Character _) = 0
depth (List []) = 1
depth (List l) = 1 + maximum (map depth l)

-- Takes a Value that is assumed to be either a scalar or a List representing
-- a rectangular array and returns its depth
-- Unlike depth, this function does not get into an infinite loop when given
-- an infinite List
--  Rectangular depth of a nonempty List is 1 plus the depth of its first
--   element
--  Rectangular depth of an empty List or a scalar is the same as its
--   regular depth
rectangularDepth :: Value -> Integer
rectangularDepth (List (r : _)) = 1 + rectangularDepth r
rectangularDepth x = depth x

-- Convert the second argument to a list of digits (least-significant first)
-- in the base given by the first argument
-- Passing the result to fromBase always gives the original number back
--  If the number is 0, the result is always empty
--  If the number is negative, convert its absolute value and then negate
--   all the digits
--  If the base is 0, the result is a singleton list containing the number
--  If the base is -1, convert to a base -1 system that uses digits 0 and 1
--  If the base is 1, convert to unary (bijective base 1)
-- TODO: algorithm that returns positive digits for negative bases
toBase :: Integer -> Integer -> [Integer]
toBase _ 0 = []
toBase 0 x = [x]
toBase (-1) x
  | x > 0 = tail $ takeCycle (2 * x) [0, 1]
  | otherwise = takeCycle (2 * abs x) [0, 1]
toBase b x
  | b > 0 && x < 0 = map (0 -) $ toBase b (abs x)
  | b == 1 = genericReplicate x 1
  | otherwise = x `mod` b : toBase b (x `div` b)

-- Taking the second argument as a list of digits (least-significant first)
-- in the base given by the first argument, convert it to a single number
fromBase :: Integer -> [Integer] -> Integer
fromBase b ds = foldr (\d n -> n * b + d) 0 ds

-- Base conversion function that takes an Integer and a list of Values
-- and returns a Value
-- Flattens second argument and treats Characters as their charcodes
valsFromBase :: Integer -> [Value] -> Value
valsFromBase b ds = Number $ fromBase b $ map scalarToInteger $ flatten ds

-- Given a list of Values, prepend a falsey Value of the same type as the
-- first element in the list
-- If the list is empty, prepend 0
consFalsey :: [Value] -> [Value]
consFalsey [] = [Number 0]
consFalsey (x : xs) = sameTypeFalsey x : x : xs

-- Given a string (List of Characters), split on newlines; given a List,
-- convert to strings and join on newlines
-- Any non-List Value is treated as a string
linesUnlines :: Value -> Value
linesUnlines = linesUnlines' . valToDisplay
  where
    linesUnlines' (AsString s) = List $ map stringToVal $ lines s
    linesUnlines' (AsList l) = stringToVal $ unlines $ map valToString l
    linesUnlines' (AsChar c) = List $ map stringToVal $ lines [c]
    linesUnlines' (AsNumber n) = List $ map (List . map Character) [show n]

-- The built-in functions are stored in a Map from names to Functions
builtins :: Map.Map String Function
builtins = Map.fromList [
  --- Arity 1 ---
  ("Abs", numberMathMonad abs),
  ("ConsFalsey", monadic $ List . consFalsey . listOrSingleton),
  ("Cycle", monadic $ List . cycle . listOrSingleton),
  ("Dec", charMathMonad pred),
  ("Depth", monadic $ Number . depth),
  ("Double", numberMathMonad (* 2)),
  ("Flatten", monadic $ List . concatMap listOrSingleton . listOrSingleton),
  ("From0", monadic $ mapOverList $ exclRange (ScalarNumber 0)),
  ("From1", monadic $ mapOverList $ exclRange (ScalarNumber 1)),
  ("Halve", numberMathMonad (`div` 2)),
  ("IFrom0", monadic $ mapOverList $ inclRange (ScalarNumber 0)),
  ("IFrom1", monadic $ mapOverList $ inclRange (ScalarNumber 1)),
  ("Id", monadic id),
  ("Inc", charMathMonad succ),
  ("Indices", monadic (\l -> List [Number i | (i, _) <- zip [0..] $ listOrSingleton l])),
  ("Length", monadic (\l -> Number $ toInteger $ length $ listOrSingleton l)),
  ("Lines", monadic linesUnlines),
  ("Neg", numberMathMonad (0 -)),
  ("Negative?", numberMathMonad $ boolToInteger . (< 0)),
  ("Odd?", numberMathMonad (`mod` 2)),   -- Alias for Parity
  ("Parity", numberMathMonad (`mod` 2)),
  ("Positive?", numberMathMonad $ boolToInteger . (> 0)),
  ("RectDepth", monadic $ Number . rectangularDepth),
  ("Show", monadic (stringToVal . show)),
  ("Sign", numberMathMonad signum),
  ("Square", numberMathMonad (\x -> x * x)),
  ("Stringify", monadic (stringToVal . valToString)),
  ("TruthyIndices", monadic (\l -> List [Number i | (i, x) <- zip [0..] $ listOrSingleton l, valToBool x])),
  ("Wrap", monadic (\x -> List [x])),
  ("Zero?", numberMathMonad $ boolToInteger . (== 0)),
  --- Arity 2 ---
  ("At", numAndListDyad $ flip genericIndex),
  ("AtCycle", numAndListDyad $ flip indexCycle),
  ("Concat", dyadic (\x y -> List $ listOrSingleton x ++ listOrSingleton y)),
  ("Cons", dyadic (\x y -> List $ x : listOrSingleton y)),
  ("Consr", dyadic (\x y -> List $ listOrSingleton y ++ [x])),
  ("Const", dyadic const),
  ("Different?", dyadic (\x y -> boolToVal $ x /= y)),  -- Alias for NotSame?
  ("Drop", numAndListDyad $ (List .) . drop'),
  ("Equal?", numberMathDyad ((boolToInteger .) . (==))),
  ("FromBase", numAndListDyad valsFromBase),
  ("Greater?", numberMathDyad ((boolToInteger .) . (>))),
  ("GreaterEqual?", numberMathDyad ((boolToInteger .) . (>=))),
  ("IDiv", numberMathDyad $ flip div),
  ("IRange", dyadic $ mapOverLists inclRange),
  ("Interleave", dyadic (\x y -> List $ interleave (listOrSingleton x) (listOrSingleton y))),
  ("Less?", numberMathDyad ((boolToInteger .) . (<))),
  ("LessEqual?", numberMathDyad ((boolToInteger .) . (<=))),
  ("Minus", charMathDyad $ flip (-)),
  ("Mod", numberMathDyad $ flip mod),
  ("NotEqual?", numberMathDyad ((boolToInteger .) . (/=))),
  ("NotSame?", dyadic (\x y -> boolToVal $ x /= y)),
  ("Pair", dyadic (\x y -> List [x, y])),
  ("Plus", charMathDyad (+)),
  ("Pow", numberMathDyad (^)),
  ("Range", dyadic $ mapOverLists exclRange),
  ("Rotate", numAndListDyad $ (List .) . rotate),
  ("Same?", dyadic (\x y -> boolToVal $ x == y)),
  ("Take", numAndListDyad $ (List .) . take'),
  ("TakeCycle", numAndListDyad $ (List .) . takeCycle),
  ("Times", charMathDyad (*)),
  ("ToBase", numToListDyad toBase),
  ("Windows", numAndListDyad (\n l -> List $ map List $ windows n l)),
  --- Arity 3 ---
  ("Trio", triadic (\x y z -> List [x, y, z])),
  --- Arity 4 ---
  ("Quartet", tetradic (\x y z w -> List [x, y, z, w]))
  ]
