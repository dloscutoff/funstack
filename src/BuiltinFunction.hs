module BuiltinFunction (
  BuiltinFunction (..),
  windows,
  fnFlatten,
  fnConcat,
  fnStringify,
  fnNot,
  fnPair,
  fnSame,
  implementation
) where

import Data.List (
  inits,
  tails,
  sort,
  nub,
  group,
  subsequences,
  unfoldr,
  genericLength,
  genericTake,
  genericDrop,
  genericIndex,
  genericReplicate
  )
import Number (Number, numerator, denominator, toNumber)
import Value (
  Value (..),
  ScalarValue (..),
  DisplayValue (..),
  fromValue,
  toValue,
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
  listMonad,
  numAndListDyad
  )

-- Built-in functions are represented by the BuiltinFunction type
data BuiltinFunction =
  Abs |
  All |
  Any |
  At |
  AtCycle |
  Chunks |
  Compare |
  Concat |
  Cons |
  ConsFalsey |
  Consr |
  Const |
  Cycle |
  Dec |
  Denominator |
  Depth |
  Div |
  Double |
  Drop |
  Equal |
  Even |
  Flatten |
  FlattenAll |
  From0 |
  From1 |
  FromBase |
  Greater |
  GreaterEqual |
  Group |
  Halve |
  Head |
  IDiv |
  IFrom0 |
  IFrom1 |
  IRange |
  Id |
  Inc |
  Indices |
  Init |
  Inits |
  Interleave |
  Last |
  Length |
  Less |
  LessEqual |
  Lines |
  Max |
  Maximum |
  Min |
  Minimum |
  Minus |
  Mod |
  Neg |
  Negative |
  Not |
  NotEqual |
  NotSame |
  Nub |
  Numerator |
  Odd |
  Pair |
  Parity |
  Partition |
  Plus |
  Positive |
  Pow |
  Product |
  Quartet |
  Range |
  Read |
  Recip |
  Repeat |
  Reverse |
  Rotate |
  Same |
  Show |
  Sign |
  Sort |
  Square |
  Stringify |
  Sum |
  Tail |
  Tails |
  Take |
  TakeCycle |
  Times |
  ToBase |
  Trio |
  TruthyIndices |
  UniformDepth |
  Windows |
  Wrap |
  Zero
  deriving (Show, Read)

-- Convert True to 1 and False to 0
boolToNumber :: Bool -> Number
boolToNumber = toNumber . fromEnum

-- Given two ScalarValues, generate an inclusive range from the first to
-- the second, coercing to the type of the second argument
inclRange :: ScalarValue -> ScalarValue -> Value
inclRange x (ScalarNumber y) = toValue [scalarToNumber x .. y]
inclRange x (ScalarChar c) = toValue [chr' n .. c]
  where n = max 0 (scalarToNumber x)

-- Given two ScalarValues, generate an exclusive range from the first to
-- the second, coercing to the type of the second argument
exclRange :: ScalarValue -> ScalarValue -> Value
exclRange x (ScalarNumber y) = toValue [scalarToNumber x .. y-1]
exclRange x (ScalarChar c)
  | c == minBound = List []
  | otherwise = toValue [chr' n .. pred c]
  where n = max 0 (scalarToNumber x)

-- Take the first n elements from a list; if n is negative, take elements
-- from the end
take' :: Number -> [a] -> [a]
take' n
  | n >= 0 = genericTake n
  | otherwise = reverse . genericTake (abs n) . reverse

-- Drop the first n elements from a list; if n is negative, drop elements
-- from the end
drop' :: Number -> [a] -> [a]
drop' n
  | n >= 0 = genericDrop n
  | otherwise = reverse . genericDrop (abs n) . reverse

-- Repeat the elements of a list infinitely; if the list is empty, return
-- empty list instead of erroring
cycle' :: [a] -> [a]
cycle' [] = []
cycle' xs = cycle xs

-- Take the first n elements from a list, repeating as necessary; if n is
-- negative, take elements starting from the end
takeCycle :: Number -> [a] -> [a]
takeCycle n
  | n >= 0 = genericTake n . cycle'
  | otherwise = reverse . genericTake (abs n) . cycle' . reverse

-- All but the last element of a list; if the list is empty, return
-- empty list instead of erroring
init' :: [a] -> [a]
init' [] = []
init' xs = init xs

-- Get the nth element in a list, repeating as necessary; if n is negative,
-- count backwards from the end
indexCycle :: [a] -> Number -> a
indexCycle l n
  | n >= 0 = genericIndex (cycle' l) n
  | otherwise = genericIndex (cycle' $ reverse l) (abs n - 1)

-- Repeat the contents of a list n times
repeat' :: Number -> [a] -> [a]
repeat' n l = concat $ genericReplicate n l

-- Rotate a list n elements to the left (to the right if n is negative)
rotate :: Number -> [a] -> [a]
rotate _ [] = []
rotate 0 l = l
rotate n l
  | minlength < abs n = rotate (n `mod` minlength) l
  | n > 0 = drop' n l ++ take' n l
  | otherwise = take' n l ++ drop' n l
  where minlength = genericLength $ take' n l

-- Get all length-n slices of a list
--  If n is negative, start from the end of the list
--  If n is zero, get all nonempty slices
windows :: Integral a => a -> [b] -> [[b]]
windows n
  | n < 0 = reverse . windows (abs n)
  | n == 0 = tail . subsequences
  | otherwise = unfoldr (\l -> if null (drop' (toNumber n - 1) l)
                               then Nothing
                               else Just (take' (toNumber n) l, tail l))

-- Take chunks (contiguous sublists) of the given sizes from a list
-- If a chunk size is negative, take from the end of the list
chunks :: [Number] -> [a] -> [[a]]
chunks [] _ = []
chunks _ [] = []
chunks (x : xs) l = take' x l : chunks xs (drop' x l)

-- Divide a list into n roughly equal-size chunks
partition :: Number -> [a] -> [[a]]
partition n l
  | n < 0 = map reverse $ partition (-n) (reverse l)
  | otherwise = chunks (chunkSizes n (genericLength l)) l
  where
    chunkSizes :: Number -> Number -> [Number]
    chunkSizes 0 _ = []
    chunkSizes count totalSize = size : chunkSizes (count - 1) (totalSize - size)
      where size = totalSize `div` count

-- Merge two lists into a single list, alternating their elements
-- Once one list runs out of elements, append the remainder of the other list
interleave :: [a] -> [a] -> [a]
interleave [] l = l
interleave (x : xs) l = x : interleave l xs

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
toBase :: Number -> Number -> [Number]
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
fromBase :: Number -> [Number] -> Number
fromBase b ds = foldr (\d n -> n * b + d) 0 ds

-- Base conversion function that takes a Number and a list of Values
-- and returns a Value
-- Flattens second argument and treats characters as their charcodes
valsFromBase :: Number -> [Value] -> Value
valsFromBase b ds = toValue $ fromBase b $ map scalarToNumber $ flattenAll ds

-- Given a list of Values, prepend a falsey Value of the same type as the
-- first element in the list
-- If the list is empty, prepend 0
consFalsey :: [Value] -> [Value]
consFalsey [] = [Scalar $ ScalarNumber 0]
consFalsey (x : xs) = sameTypeFalsey x : x : xs

-- Given a string (List of ScalarChars), split on newlines; given a List,
-- convert to strings and join on newlines
-- Any non-List Value is treated as a string
linesUnlines :: Value -> Value
linesUnlines = linesUnlines' . fromValue
  where
    linesUnlines' (AsString s) = toValue $ lines s
    linesUnlines' (AsList l) = toValue $ unlines $ map valToString l
    linesUnlines' (AsChar c) = toValue $ lines [c]
    linesUnlines' (AsNumber n) = toValue $ lines $ show n

-- The following builtins are used elsewhere besides just being included in
-- the builtins Map, so they are defined separately
fnFlatten :: Function
fnFlatten = listMonad flattenOnce

fnConcat :: Function
fnConcat = dyadic (\x y -> toValue (fromValue x ++ fromValue y :: [Value]))

fnStringify :: Function
fnStringify = monadic $ toValue . valToString

fnNot :: Function
fnNot = monadic $ toValue . not . fromValue

fnPair :: Function
fnPair = dyadic (\x y -> toValue [x, y])

fnSame :: Function
fnSame = dyadic $ (toValue .) . (==)

-- Given a BuiltinFunction, return the Function that it represents
implementation :: BuiltinFunction -> Function
implementation f = case f of
  --- Arity 1 ---
  Abs -> numberMathMonad abs
  All -> listMonad $ all fromValue
  Any -> listMonad $ any fromValue
  ConsFalsey -> listMonad consFalsey
  Cycle -> listMonad cycle'
  Dec -> charMathMonad pred
  Denominator -> numberMathMonad $ fromInteger . denominator
  Depth -> monadic $ Scalar . ScalarNumber . depth
  Double -> numberMathMonad (* 2)
  Even -> numberMathMonad $ boolToNumber . even
  Flatten -> fnFlatten
  FlattenAll -> listMonad flattenAll
  From0 -> monadic $ mapOverList $ exclRange (ScalarNumber 0)
  From1 -> monadic $ mapOverList $ exclRange (ScalarNumber 1)
  Group -> listMonad group
  Halve -> numberMathMonad (`div` 2)
  Head -> listMonad head
  IFrom0 -> monadic $ mapOverList $ inclRange (ScalarNumber 0)
  IFrom1 -> monadic $ mapOverList $ inclRange (ScalarNumber 1)
  Id -> monadic id
  Inc -> charMathMonad succ
  Indices -> listMonad (\l -> [Scalar $ ScalarNumber i | (i, _) <- zip [0..] l])
  Init -> listMonad init'
  Inits -> listMonad inits
  Recip -> numberMathMonad recip
  Last -> listMonad last
  Length -> monadic (\l -> Scalar $ ScalarNumber $ genericLength $ listOrString l)
  Lines -> monadic linesUnlines
  Maximum -> listMonad $ maximum . map toValue . flattenAll
  Minimum -> listMonad $ minimum . map toValue . flattenAll
  Neg -> numberMathMonad (0 -)
  Negative -> numberMathMonad $ boolToNumber . (< 0)
  Not -> fnNot
  Nub -> listMonad nub
  Numerator -> numberMathMonad $ fromInteger . numerator
  Odd -> numberMathMonad $ boolToNumber . odd
  Parity -> numberMathMonad (`mod` 2)
  Positive -> numberMathMonad $ boolToNumber . (> 0)
  Product -> monadic $ Scalar . ScalarNumber . product . toNumberList
  Read -> monadic $ read . valToString
  Reverse -> listMonad reverse
  Show -> monadic $ toValue . show
  Sign -> numberMathMonad signum
  Sort -> listMonad sort
  Square -> numberMathMonad (\x -> x * x)
  Stringify -> fnStringify
  Sum -> monadic $ Scalar . ScalarNumber . sum . toNumberList
  Tail -> listMonad $ drop 1
  Tails -> listMonad tails
  TruthyIndices -> listMonad (\l -> [Scalar $ ScalarNumber i | (i, x) <- zip [0..] l, fromValue x :: Bool])
  UniformDepth -> monadic $ Scalar . ScalarNumber . uniformDepth
  Wrap -> monadic (\x -> toValue [x])
  Zero -> numberMathMonad $ boolToNumber . (== 0)
  --- Arity 2 ---
  At -> numAndListDyad $ flip genericIndex
  AtCycle -> numAndListDyad $ flip indexCycle
  Chunks -> dyadic (\x y -> toValue $ chunks (cycle' $ toNumberList x) (fromValue y :: [Value]))
  Compare -> dyadic (\x y -> toValue $ y `compare` x)
  Concat -> fnConcat
  Cons -> dyadic (\x y -> toValue $ x : fromValue y)
  Consr -> dyadic (\x y -> toValue $ fromValue y ++ [x])
  Const -> dyadic const
  Div -> numberMathDyad $ flip (/)
  Drop -> numAndListDyad $ (toValue .) . drop'
  Equal -> numberMathDyad $ (boolToNumber .) . (==)
  FromBase -> numAndListDyad valsFromBase
  Greater -> numberMathDyad $ (boolToNumber .) . flip (>)
  GreaterEqual -> numberMathDyad $ (boolToNumber .) . flip (>=)
  IDiv -> numberMathDyad $ flip div
  IRange -> dyadic $ mapOverLists inclRange
  Interleave -> dyadic (\x y -> toValue $ interleave (fromValue x) (fromValue y :: [Value]))
  Less -> numberMathDyad $ (boolToNumber .) . flip (<)
  LessEqual -> numberMathDyad $ (boolToNumber .) . flip (<=)
  Max -> dyadic max
  Min -> dyadic min
  Minus -> charMathDyad $ flip (-)
  Mod -> numberMathDyad $ flip mod
  NotEqual -> numberMathDyad $ (boolToNumber .) . (/=)
  NotSame -> dyadic $ (toValue .) . (/=)
  Pair -> fnPair
  Partition -> numAndListDyad $ (toValue .) . partition
  Plus -> charMathDyad (+)
  Pow -> numberMathDyad (^)
  Range -> dyadic $ mapOverLists exclRange
  Repeat -> numAndListDyad $ (toValue .) . repeat'
  Rotate -> numAndListDyad $ (toValue .) . rotate
  Same -> fnSame
  Take -> numAndListDyad $ (toValue .) . take'
  TakeCycle -> numAndListDyad $ (toValue .) . takeCycle
  Times -> charMathDyad (*)
  ToBase -> numToListDyad toBase
  Windows -> numAndListDyad (\n l -> toValue $ windows n l)
  --- Arity 3 ---
  Trio -> triadic (\x y z -> toValue [x, y, z])
  --- Arity 4 ---
  Quartet -> tetradic (\x y z w -> toValue [x, y, z, w])
