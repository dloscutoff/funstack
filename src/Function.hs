module Function (
  Function (..),
  Arity,
  arity,
  extract,
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
  numAndListDyad
) where

import Value (
  Value (..),
  ScalarValue (..),
  ord',
  chr',
  scalarToInteger,
  listOrSingleton
  )

-- The Arity of a function is an Int
type Arity = Int

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

-- If the Function is a Constant, return its value; otherwise, return Nothing
extract :: Function -> Maybe Value
extract (Constant x) = Just x
extract _ = Nothing

-- Functions are not normally shown, but can be in some error messages
instance Show Function where
  show (Constant x) = "<const " ++ show x ++ ">"
  show (Function a _) = "<arity-" ++ show a ++ " function>"

-- Functions form a semigroup, where <> is generalized function composition:
--  Composing two Constants returns the first one
--  Composing any non-Constant Function with a Constant binds the Constant's
--   value to the first argument of the Function
--  Composing any Function f with a non-Constant Functions g results in
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
applyFully :: Function -> [Value] -> Value
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

-- Given a one-argument Hakell function that takes ScalarValues, return a new
-- function that applies that function to non-List arguments and maps the
-- function over List arguments
mapOverList :: (ScalarValue -> Value) -> Value -> Value
mapOverList f (List l) = List $ map (mapOverList f) l
mapOverList f (Number x) = f (ScalarNumber x)
mapOverList f (Character c) = f (ScalarChar c)

-- Given a two-argument Hakell function that takes ScalarValues, return
-- a new function that applies that function to non-List arguments and maps
-- the function over List arguments
-- TODO: should this use some kind of zip-longest logic instead of zipWith?
mapOverLists :: (ScalarValue -> ScalarValue -> Value) -> Value -> Value -> Value
mapOverLists f (List l) (List m) = List $ zipWith (mapOverLists f) l m
mapOverLists f x (List l) = List $ map (mapOverLists f x) l
mapOverLists f (List l) y = List $ map ((flip $ mapOverLists f) y) l
mapOverLists f (Number x) (Number y) = f (ScalarNumber x) (ScalarNumber y)
mapOverLists f (Number x) (Character c) = f (ScalarNumber x) (ScalarChar c)
mapOverLists f (Character c) (Number x) = f (ScalarChar c) (ScalarNumber x)
mapOverLists f (Character c) (Character d) = f (ScalarChar c) (ScalarChar d)

-- Given a two-argument Hakell function that takes a ScalarValue and a Value,
-- return a new function that applies that function to its arguments if the
-- first argument is not a List and maps over a first argument that is a List
mapOverLeftList :: (ScalarValue -> Value -> Value) -> Value -> Value -> Value
mapOverLeftList f (List l) y = List [mapOverLeftList f x y | x <- l]
mapOverLeftList f (Number x) y = f (ScalarNumber x) y
mapOverLeftList f (Character c) y = f (ScalarChar c) y

-- Convert a function from Integer to Integer to a function from ScalarValue
-- to Value that, given a ScalarChar, applies the original function to its
-- charcode and converts the result back to a Character
unaryCharMath :: (Integer -> Integer) -> ScalarValue -> Value
unaryCharMath f (ScalarNumber x) = Number $ f x
unaryCharMath f (ScalarChar c) = Character $ chr' $ f $ ord' c

-- Convert a two-argument function over Integers to a function from two
-- ScalarValues to Value that, given a Character, applies the original
-- function to its charcode and:
--  Given one Character and one Number, converts the result to a Character
--  Given two Characters or two Numbers, converts the result to a Number
binaryCharMath :: (Integer -> Integer -> Integer) -> ScalarValue -> ScalarValue -> Value
binaryCharMath f (ScalarNumber x) (ScalarNumber y) = Number $ f x y
binaryCharMath f (ScalarNumber x) (ScalarChar c) = Character $ chr' $ f x (ord' c)
binaryCharMath f (ScalarChar c) (ScalarNumber x) = Character $ chr' $ f (ord' c) x
binaryCharMath f (ScalarChar c) (ScalarChar d) = Number $ f (ord' c) (ord' d)

-- Given a one-argument function over Integers, return a monadic Function that
-- takes Numbers to Numbers, Characters to Characters, and maps over Lists
charMathMonad :: (Integer -> Integer) -> Function
charMathMonad = monadic . mapOverList . unaryCharMath

-- Given a two-argument function over Integers, return a dyadic Function that
-- takes Number + Number or Character + Character to Number, Number +
-- Character or Character + Number to Character, and maps over Lists
charMathDyad :: (Integer -> Integer -> Integer) -> Function
charMathDyad = dyadic . mapOverLists . binaryCharMath

-- Given a one-argument function over Integers, return a monadic Function that
-- treats Characters as their charcodes and maps over Lists
numberMathMonad :: (Integer -> Integer) -> Function
numberMathMonad = monadic . mapOverList . unaryNumberMath
  where unaryNumberMath f = Number . f . scalarToInteger

-- Given a two-argument function over Integers, return a dyadic Function that
-- treats Characters as their charcodes and maps over Lists
numberMathDyad :: (Integer -> Integer -> Integer) -> Function
numberMathDyad = dyadic . mapOverLists . binaryNumberMath
  where binaryNumberMath f x y = Number $ f (scalarToInteger x) (scalarToInteger y)

-- Given a one-argument function from Integer to [Integer], return a monadic
-- Function that treats Characters as their charcodes and maps over Lists
numToListMonad :: (Integer -> [Integer]) -> Function
numToListMonad = monadic . mapOverList . unaryNumToList
  where unaryNumToList f = List . map Number . f . scalarToInteger

-- Given a two-argument function from Integers to [Integer], return a dyadic
-- Function that treats Characters as their charcodes and maps over Lists
numToListDyad :: (Integer -> Integer -> [Integer]) -> Function
numToListDyad = dyadic . mapOverLists . binaryNumToList
  where binaryNumToList f x y = List $ map Number $ f (scalarToInteger x) (scalarToInteger y)

-- Given a two-argument function from Integer and [Value] to Value, return
-- a dyadic Function:
--  First argument: treat Characters as their charcodes and map over Lists
--  Second argument: convert non-List to singleton List
numAndListDyad :: (Integer -> [Value] -> Value) -> Function
numAndListDyad = dyadic . mapOverLeftList . numAndListToList
  where numAndListToList f x y = f (scalarToInteger x) (listOrSingleton y)
