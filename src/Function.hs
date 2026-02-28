module Function (
  Function (..),
  Arity,
  ArgList,
  arity,
  bind,
  bind2,
  bindSecond,
  rbind,
  apply,
  apply2,
  applyFully,
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
  numToListMonad,
  numToListDyad,
  listMonad,
  numAndListDyad
) where

import Value (
  Value (..),
  ScalarValue (..),
  fromValue,
  ToValue,
  toValue,
  ord',
  chr',
  scalarToInteger
  )

-- The Arity of a function is an Int
type Arity = Int

-- The ArgList of a function is a list of Values
type ArgList = [Value]

-- Function represents a curried function:
--  Constant is a wrapper around a Value, which could be thought of as
--   representing a 0-arity function that returns a constant
--  Function represents a function with arity higher than 0 that can be
--   bound to a Value, resulting in a new Function (possibly a Constant)
data Function =
  Constant Value |
  Function Arity (Value -> Function)

-- Return the arity of any Function, giving 0 for a Constant
arity :: Function -> Arity
arity (Constant _) = 0
arity (Function a _) = a

-- Functions are not normally shown, but can be in some error messages
instance Show Function where
  show (Constant x) = "<const " ++ show x ++ ">"
  show (Function a _) = "<arity-" ++ show a ++ " function>"

-- Functions form a semigroup, where <> is generalized function composition:
--  Composing two Constants returns the first one
--  Composing any non-Constant Function with a Constant binds the Constant's
--   value to the first argument of the Function
--  Composing any Function f with a non-Constant Function g results in
--   a new Function that binds its first argument to g and composes f with
--   the result
instance Semigroup Function where
  (Constant x) <> (Constant _) = Constant x
  (Function _ fbind) <> (Constant y) = fbind y
  f <> (Function b gbind) = Function (arity f + b - 1) ((f <>) . gbind)

-- Functions form a monoid, where mempty is the identity function (a function
-- that takes one argument and returns a Constant containing that value)
instance Monoid Function where
  mempty = Function 1 Constant

-- Bind an argument to a Function
-- This also works if the function is a Constant (leaving it unchanged)
bind :: Function -> Value -> Function
bind f = (f <>) . Constant

-- Bind two arguments to a Function
bind2 :: Function -> Value -> Value -> Function
bind2 f x y = bind (bind f x) y

-- Bind a Value to the second argument of a Function
-- If the Function's arity is less than two, leave it unchanged
bindSecond :: Function -> Value -> Function
bindSecond f x
  | arity f < 2 = f
  | otherwise = Function (arity f - 1) (\y -> bind2 f y x)

-- Bind a Value to the rightmost argument of a Function
rbind :: Function -> Value -> Function
rbind f x
  | arity f < 2 = bind f x
  | otherwise = Function (arity f - 1) (\y -> rbind (bind f y) x)

-- Apply a Function to one argument and return a Value
-- Partial; errors when given a function of arity 2 or greater
apply :: Function -> Value -> Value
apply f x
  | (Constant r) <- bind f x = r
  | otherwise = error $
      "Cannot apply " ++
      show f ++
      " to one argument (perhaps you meant to use bind instead?)"

-- Apply a Function to two arguments and return a Value
-- Partial; errors when given a function of arity 3 or greater
apply2 :: Function -> Value -> Value -> Value
apply2 f x y
  | (Constant r) <- bind2 f x y = r
  | otherwise = error $
      "Cannot apply " ++
      show f ++
      " to two arguments (perhaps you meant to use bind2 instead?)"

-- Apply a Function to a list of arguments and return a Value
-- If the length of the argument list does not match the arity of the
-- Function, cycle the argument list and trim to the correct length
-- The only time this doesn't work is if the argument list is empty, which
-- gives an error
applyFully :: Function -> ArgList -> Value
applyFully (Constant x) _ = x
applyFully _ [] = error "Cannot apply function to empty arglist"
applyFully f args
  | arity f /= length args = applyFully f $ take (arity f) (cycle args)
  | (Constant r) <- result = r
  | otherwise = error $  -- This shouldn't happen, but leave in for debugging
      "Error in applyFully " ++ show f ++
      " " ++ show args ++
      ": returned " ++ show result
  where result = foldl bind f args

-- Convert a one-argument Haskell function to an arity-1 Function
monadic :: (Value -> Value) -> Function
monadic f = Function 1 (Constant . f)

-- Convert a two-argument Haskell function to an arity-2 Function
dyadic :: (Value -> Value -> Value) -> Function
dyadic f = Function 2 (monadic . f)

-- Convert a three-argument Haskell function to an arity-3 Function
triadic :: (Value -> Value -> Value -> Value) -> Function
triadic f = Function 3 (dyadic . f)

-- Convert a four-argument Haskell function to an arity-4 Function
tetradic :: (Value -> Value -> Value -> Value -> Value) -> Function
tetradic f = Function 4 (triadic . f)

-- Given a one-argument Haskell function that takes ScalarValues, return a new
-- function that applies that function to non-List arguments and maps the
-- function over List arguments
mapOverList :: (ScalarValue -> Value) -> Value -> Value
mapOverList f (List l) = toValue $ map (mapOverList f) l
mapOverList f (Scalar x) = f x

-- Given a two-argument Haskell function that takes ScalarValues, return
-- a new function that applies that function to non-List arguments and maps
-- the function over List arguments
-- TODO: use some kind of zip-longest logic instead of zipWith
mapOverLists :: (ScalarValue -> ScalarValue -> Value) -> Value -> Value -> Value
mapOverLists f (List l) (List m) = toValue $ zipWith (mapOverLists f) l m
mapOverLists f x (List l) = toValue $ map (mapOverLists f x) l
mapOverLists f (List l) y = toValue $ map ((flip $ mapOverLists f) y) l
mapOverLists f (Scalar x) (Scalar y) = f x y

-- Given a two-argument Haskell function that takes a ScalarValue and a Value,
-- return a new function that applies that function to its arguments if the
-- first argument is not a List and maps over a first argument that is a List
mapOverLeftList :: (ScalarValue -> Value -> Value) -> Value -> Value -> Value
mapOverLeftList f (List l) y = toValue $ map ((flip $ mapOverLeftList f) y) l
mapOverLeftList f (Scalar x) y = f x y

-- Convert a function from Integer to Integer to a function from ScalarValue
-- to Value that, given a ScalarChar, applies the original function to its
-- charcode and converts the result back to a ScalarChar
unaryCharMath :: (Integer -> Integer) -> ScalarValue -> Value
unaryCharMath f (ScalarNumber x) = Scalar $ ScalarNumber $ f x
unaryCharMath f (ScalarChar c) = Scalar $ ScalarChar $ chr' $ f $ ord' c

-- Convert a two-argument function over Integers to a function from two
-- ScalarValues to Value that applies the original function to the underlying
-- numbers or charcodes and:
--  Given one ScalarChar and one ScalarNumber, converts the result to a ScalarChar
--  Given two ScalarChar or two ScalarNumbers, converts the result to a ScalarNumber
binaryCharMath :: (Integer -> Integer -> Integer) -> ScalarValue -> ScalarValue -> Value
binaryCharMath f (ScalarNumber x) (ScalarNumber y) = Scalar $ ScalarNumber $ f x y
binaryCharMath f (ScalarNumber x) (ScalarChar c) = Scalar $ ScalarChar $ chr' $ f x (ord' c)
binaryCharMath f (ScalarChar c) (ScalarNumber x) = Scalar $ ScalarChar $ chr' $ f (ord' c) x
binaryCharMath f (ScalarChar c) (ScalarChar d) = Scalar $ ScalarNumber $ f (ord' c) (ord' d)

-- Given a one-argument function over Integers, return a monadic Function that
-- takes numbers to numbers, chars to chars, and maps over Lists
charMathMonad :: (Integer -> Integer) -> Function
charMathMonad = monadic . mapOverList . unaryCharMath

-- Given a two-argument function over Integers, return a dyadic Function that
-- takes number + number or char + char to number, number + char
-- or char + number to char, and maps over Lists
charMathDyad :: (Integer -> Integer -> Integer) -> Function
charMathDyad = dyadic . mapOverLists . binaryCharMath

-- Given a one-argument function over Integers, return a monadic Function that
-- treats chars as their charcodes and maps over Lists
numberMathMonad :: (Integer -> Integer) -> Function
numberMathMonad = monadic . mapOverList . unaryNumberMath
  where unaryNumberMath f x = toValue $ f (scalarToInteger x)

-- Given a two-argument function over Integers, return a dyadic Function that
-- treats chars as their charcodes and maps over Lists
numberMathDyad :: (Integer -> Integer -> Integer) -> Function
numberMathDyad = dyadic . mapOverLists . binaryNumberMath
  where binaryNumberMath f x y = toValue $ f (scalarToInteger x) (scalarToInteger y)

-- Given a one-argument function from Integer to [Integer], return a monadic
-- Function that treats chars as their charcodes and maps over Lists
numToListMonad :: (Integer -> [Integer]) -> Function
numToListMonad = monadic . mapOverList . unaryNumToList
  where unaryNumToList f x = toValue $ f (scalarToInteger x)

-- Given a two-argument function from Integers to [Integer], return a dyadic
-- Function that treats chars as their charcodes and maps over Lists
numToListDyad :: (Integer -> Integer -> [Integer]) -> Function
numToListDyad = dyadic . mapOverLists . binaryNumToList
  where binaryNumToList f x y = toValue $ f (scalarToInteger x) (scalarToInteger y)

-- Given a one-argument function from [Value] to some type convertible to Value,
-- return a monadic Function that applies to Lists and wraps Scalars in
-- singleton lists
listMonad :: (ToValue a) => ([Value] -> a) -> Function
listMonad f = monadic $ toValue . f . fromValue

-- Given a two-argument function from Integer and [Value] to Value, return
-- a dyadic Function:
--  First argument: treat chars as their charcodes and map over Lists
--  Second argument: convert non-List to singleton List
numAndListDyad :: (Integer -> [Value] -> Value) -> Function
numAndListDyad = dyadic . mapOverLeftList . numAndListToList
  where numAndListToList f x y = f (scalarToInteger x) (fromValue y)
