module BuiltinModifiers (
  modifiers
) where

import Data.List (unfoldr)
import qualified Data.Map as Map
import Value (Value (..), listOrSingleton, valToBool)
import Function (
  Function (..),
  Arity,
  arity,
  bind,
  bind2,
  bindSecond,
  rbind,
  apply,
  apply2,
  applyFully,
  monadic,
  dyadic
  )
import Modifier (Modifier (..))
import qualified BuiltinFunctions as Builtin

-- Compose two Functions
-- This is merely an alias for the binary operator of the Function semigroup
compose2 :: Function -> Function -> Function
compose2 = (<>)

-- Compose three Functions
compose3 :: Function -> Function -> Function -> Function
compose3 f g h = f <> g <> h

-- Compose four Functions
compose4 :: Function -> Function -> Function -> Function -> Function
compose4 f g h i = f <> g <> h <> i

-- Compose two Functions such that the result of the second is passed as
-- the last argument of the first
rcompose2 :: Function -> Function -> Function
rcompose2 f g
  | arity f == 1 = f <> g
  | otherwise = Function (arity f + arity g - 1) (\x -> bind f x `rcompose2` g)

-- Compose three Functions such that the result of each is passed as the
-- last argument of the one before it
rcompose3 :: Function -> Function -> Function -> Function
rcompose3 f g h = f `rcompose2` g `rcompose2` h

-- Pass the same arguments to both Functions, but pass the result of the
-- second as the last argument of the first
hook :: Function -> Function -> Function
hook f (Constant x) = rbind f x
hook f g
  | arity f <= 1 = f <> g
  | arity f == 2 && arity g == 1 = Function 1 (\x -> bind2 f x (apply g x))
  | otherwise = Function a' (\x -> hook (bind f x) (bind g x))
  where a' = max (arity f - 1) (arity g)

-- Helper function for over (see below)
over' :: Function -> Function -> Function -> Function
over' f g g'
  | arity g' == 1 = Function a' (\x -> over (bind f (apply g' x)) g)
  | otherwise = Function a' (\x -> over' f g (bind g' x))
  where a' = (arity f - 1) * (arity g) + (arity g')

-- Given an arity-N Function and an arity-M Function, return an arity-N*M
-- Function: pass N groups of M arguments each through the second Function;
-- then pass those N results as arguments to the first Function
over :: Function -> Function -> Function
over f g
  | arity f <= 1 = f <> g
  | otherwise = over' f g g

-- Pass the same arguments to three different Functions: if the result from
-- the first Function is truthy, use the result from the second Function;
-- otherwise, use the result from the third Function
-- The unused Function is never called
ifThenElse :: Function -> Function -> Function -> Function
ifThenElse f g h = ifThenElse' a f g h
  where
    a = maximum $ map arity [f, g, h]
    ifThenElse' a' f' g' h'
      | a' == 1 = Function 1 (\x -> if (valToBool $ apply f' x) then (bind g' x) else (bind h' x))
      | otherwise = Function a' (\x -> ifThenElse' (a' - 1) (bind f' x) (bind g' x) (bind h' x))

-- Modify a Function to take its first argument second and its second
-- argument first
-- If the original Function had arity 1, it becomes an arity-2 Function
-- that ignores its first argument
flipArgs :: Function -> Function
flipArgs f = Function (max 2 (arity f)) (\x -> bindSecond f x)

-- Modify a Function to take its last argument first
rotateArgs :: Function -> Function
rotateArgs f = Function (arity f) (\x -> rbind f x)

-- Given a Function, return a new Function that works as follows:
--  Collect a given number of arguments in a list
--  Once the full number of arguments has been collected, apply the original
--   Function to the collected arguments using the given application function
collectArgs :: Function -> Arity -> (Function -> [Value] -> Value) -> [Value] -> Value -> Function
collectArgs f a applyFn args arg
  | a <= 1 = Constant $ applyFn f $ reverse (arg : args)
  | otherwise = Function (a - 1) $ collectArgs f (a - 1) applyFn (arg : args)

-- Modify a Function to have the given arity
--  If the new arity is the same as the old arity, no change is made
--  If the new arity is greater than the old arity, the extra arguments
--   of the new Function are simply ignored
--  If the new arity is less than the old arity, the arguments of the new
--   Function are cycled to provide enough arguments for the original Function
convertArity :: Arity -> Function -> Function
convertArity a' (Constant x)
  | a' < 1 = Constant x
  | otherwise = Function a' (\_ -> convertArity (a' - 1) (Constant x))
convertArity a' f
  | a' < 1 = error "Cannot convert function to arity less than 1"
  | a' == arity f = f
  | otherwise = Function a' $ collectArgs f a' applyFully []

-- Modify a Function to generate an infinite List by repeated appliction
-- The new Function has the same arity (call it N) as the original Function
-- The resulting List begins with the arguments unchanged; thereafter, each
-- element is the result of applying the Function over the preceding N
-- elements
iterate' :: Function -> Function
iterate' f = Function (arity f) (collectArgs f (arity f) iterateApply [])
  where
    iterateStep f' args = Just (head args, tail args ++ [applyFully f' args])
    iterateApply f' = List . unfoldr (iterateStep f')

-- Same as iterate', but stop when the results stop changing
-- For higher-arity Functions, the entire argument list must remain unchanged
-- to stop the iteration
fixiter :: Function -> Function
fixiter f = Function (arity f) (collectArgs f (arity f) fixiterApply [])
  where
    fixiterApply' f' args
      | all (== newVal) args = args
      | otherwise = head args : fixiterApply' f' (tail args ++ [newVal])
      where newVal = applyFully f' args
    fixiterApply f' = List . fixiterApply' f'

-- Same as fixiter, but only return the final result, not the whole list
fixpoint :: Function -> Function
fixpoint f = Function (arity f) (collectArgs f (arity f) fixpointApply [])
  where
    fixpointApply f' args
      | all (== newVal) args = newVal
      | otherwise = fixpointApply f (tail args ++ [newVal])
      where newVal = applyFully f' args

-- Given a Function and a list of Values, take elements from the list while
-- applying the Function to them results in a truthy Value
-- For higher-arity Functions, include elements up to but not including
-- the first element that makes the predicate falsey
takeWhile' :: Function -> [Value] -> [Value]
takeWhile' f xs
  | length args < arity f = xs
  | valToBool (applyFully f args) = head xs : takeWhile' f (tail xs)
  | otherwise = init args
  where args = take (arity f) xs

-- Given a Function and a list of Values, drop elements from the list while
-- applying the Function to them results in a truthy Value
-- For higher-arity Functions, discard elements up to but not including
-- the first element that makes the predicate falsey
dropWhile' :: Function -> [Value] -> [Value]
dropWhile' f xs
  | length args < arity f = []
  | valToBool (applyFully f args) = dropWhile' f (tail xs)
  | otherwise = drop (arity f - 1) xs
  where args = take (arity f) xs

-- Bind multiple Functions to an argument and return a list of new Functions
parallelBind :: [Function] -> Value -> [Function]
parallelBind fs x = [bind f x | f <- fs]

-- Bind multiple Functions to an argument and return a list of new Functions;
-- if the argument is a List, zip the Functions with the elements of the
-- argument; otherwise, bind each Function to the argument
zipBind :: [Function] -> Value -> [Function]
zipBind fs (List l) = zipWith bind fs l
zipBind fs x = parallelBind fs x

-- Apply multiple arity-1 Functions to an argument and return a List of
-- the results
parallelApply :: [Function] -> Value -> Value
parallelApply fs x = List [apply f x | f <- fs]

-- Apply multiple arity-1 Functions to an argument and return a List of
-- the results; if the argument is a List, zip the Functions with the
-- elements of the argument; otherwise, apply each Function to the argument
zipApply :: [Function] -> Value -> Value
zipApply fs (List l) = List $ zipWith apply fs l
zipApply fs x = parallelApply fs x

-- Given an arity and a list of Functions of that arity, turn them into a
-- single Function that applies each of the constituent Functions in parallel
-- and returns a List of the results
parallel :: Arity -> [Function] -> Function
parallel a fs
  | a < 1 = error $ "Cannot call parallel with arity " ++ show a ++ " less than 1"
  | a == 1 = Function 1 (Constant . parallelApply fs)
  | otherwise = Function a (parallel (a - 1) . parallelBind fs)

-- Given an arity and a list of Functions of that arity, turn them into a
-- single Function that applies each of the constituent Functions in parallel
-- and returns a List of the results
zipParallel :: Arity -> [Function] -> Function
zipParallel a fs
  | a < 1 = error $ "Cannot call zipParallel with arity " ++ show a ++ " less than 1"
  | a == 1 = Function 1 (Constant . zipApply fs)
  | otherwise = Function a (zipParallel (a - 1) . zipBind fs)

-- Convert a Function to map over its arguments
-- If the Function takes multiple arguments, zip the argument lists
-- together (truncating to the length of the shortest one)
-- However, if an argument is not a List, it is used across the board
-- If all arguments are non-Lists, the result is a singleton List
mapZipping :: Function -> Function
mapZipping f
  | arity f == 1 =
      monadic (\xs -> List [apply f x | x <- listOrSingleton xs])
  | otherwise = Function (arity f) mapBind
  where
    mapBind (List l) = zipParallel (arity f - 1) (map (bind f) l)
    mapBind x = mapZipping $ bind f x

-- Convert a Function to map over its first argument
-- If the Function only takes one argument, apply it to each element of its
-- argument, pair the result with the original element, and return a List
-- of those pairs
-- If the first argument is not a List, wrap it in a singleton List
mapLeft :: Function -> Function
mapLeft f
  | a == 1 =
      monadic (\xs -> List [List [apply f x, x] | x <- listOrSingleton xs])
  | otherwise =
      Function a (\xs -> parallel (a - 1) [bind f x | x <- listOrSingleton xs])
  where a = arity f

-- Convert a Function to map over its last argument
-- If the Function only takes one argument, pair each element of its original
-- argument with the result of applying the function to that element, and 
-- return a List of those pairs
-- If the first argument is not a List, wrap it in a singleton List
mapRight :: Function -> Function
mapRight f
  | a == 1 =
      monadic (\xs -> List [List [x, apply f x] | x <- listOrSingleton xs])
  | a == 2 =
      dyadic (\x ys -> List [apply2 f x y | y <- listOrSingleton ys])
  | otherwise = Function a (\x -> mapRight $ bind f x)
  where a = arity f

-- Convert an arity-N Function to a Function that creates a depth-N nested
-- List containing the results of applying the original Function to every
-- combination of values from its arguments (N-dimensional outer product)
table :: Function -> Function
table f
  | a < 1 = f
  | a == 1 = mapZipping f
  | otherwise =
      Function a (\xs -> parallel (a - 1) [table $ bind f x | x <- listOrSingleton xs])
  where a = arity f

-- Given a Function, an intial accumulator Value, and a list of Values,
-- perform a generalized left scan
-- At each step, the accumulator is passed as the first argument of the
-- Function, followed by the first N-1 elements of the list, where N is
-- the Function's arity
-- Once there are not enough elements left in the list, the process stops
scanl' :: Function -> Value -> [Value] -> [Value]
scanl' f x l
  | arity f <= 1 = error "Function for scanl must have arity >= 2"
  | length args < argnum = [x]
  | otherwise = x : scanl' f accum remainder
  where
    argnum = arity f - 1
    args = take argnum l
    remainder = drop argnum l
    accum = applyFully f (x : args)

-- Same as scanl', but use the first element of the list as the initial
-- accumulator value; if the list is empty, return empty list
scanl1' :: Function -> [Value] -> [Value]
scanl1' f l
  | arity f <= 1 = error "Function for scanl1 must have arity >= 2"
  | null l = []
  | otherwise = scanl' f (head l) (tail l)

-- Given a Function, an intial accumulator Value, and a list of Values,
-- perform a generalized right scan
-- At each step, the first N-1 elements of the list (where N is the
-- Function's arity) plus the result of a recursive call are passed as
-- arguments to the Function
-- Once there are not enough elements left in the list, the process stops
scanr' :: Function -> Value -> [Value] -> [Value]
scanr' f x l
  | arity f <= 1 = error "Function for scanr must have arity >= 2"
  | length args < argnum = [x]
  | otherwise = applyFully f (args ++ take 1 accum) : accum
  where
    argnum = arity f - 1
    args = take argnum l
    remainder = drop argnum l
    accum = scanr' f x remainder

-- Same as scanr', but use the last element of the list as the initial
-- accumulator value; if the list is empty, return empty list
-- If the arity of the Function is greater than 2, the initial accumulator
-- may not be the last element of the list per se, but rather the k*(N-1)st
-- element (0-indexed) for some k, where N is the arity of the Function
scanr1' :: Function -> [Value] -> [Value]
scanr1' f l
  | arity f <= 1 = error "Function for scanr1 must have arity >= 2"
  | null l = []
  | null remainder = take 1 l
  | otherwise = applyFully f (args ++ take 1 accum) : accum
  where
    argnum = arity f - 1
    args = take argnum l
    remainder = drop argnum l
    accum = scanr1' f remainder

-- Given a Function, an intial accumulator Value, and a list of Values,
-- perform a generalized left fold by taking the last element of the left scan
foldl' :: Function -> Value -> [Value] -> Value
foldl' f x l
  | arity f <= 1 = error "Function for foldl must have arity >= 2"
  | otherwise = last $ scanl' f x l

-- Same as foldl', but use the first element of the list as the initial
-- accumulator value
-- If the list is empty, error
foldl1' :: Function -> [Value] -> Value
foldl1' f l
  | arity f <= 1 = error "Function for foldl1 must have arity >= 2"
  | null l = error $ "Cannot foldl1 empty list"
  | otherwise = last $ scanl1' f l

-- Given a Function, an intial accumulator Value, and a list of Values,
-- perform a generalized right fold by taking the first element of the
-- right scan
foldr' :: Function -> Value -> [Value] -> Value
foldr' f x l
  | arity f <= 1 = error "Function for foldr must have arity >= 2"
  | otherwise = head $ scanr' f x l

-- Same as foldr', but use the last element of the list as the initial
-- accumulator value
-- If the arity of the Function is greater than 2, the initial accumulator
-- may not be the last element of the list per se, but rather the k*(N-1)st
-- element (0-indexed) for some k, where N is the arity of the Function
-- If the list is empty, error
foldr1' :: Function -> [Value] -> Value
foldr1' f l
  | arity f <= 1 = error "Function for foldr1 must have arity >= 2"
  | null l = error $ "Cannot foldr1 empty list"
  | otherwise = head $ scanr1' f l

-- The built-in modifiers are stored in a Map from names to Modifiers
modifiers :: Map.Map String Modifier
modifiers = Map.fromList [
  --- 1-modifiers ---
  ("dropwhile", Modifier1 (\f -> monadic (List . dropWhile' f . listOrSingleton))),
  ("fixiter", Modifier1 fixiter),
  ("fixpoint", Modifier1 fixpoint),
  ("flatmap", Modifier1 (\f -> Builtin.fnFlatten <> mapZipping f)),
  ("flip", Modifier1 flipArgs),
  ("foldl", Modifier1 (\f -> dyadic (\x ys -> foldl' f x $ listOrSingleton ys))),
  ("foldl1", Modifier1 (\f -> monadic (\xs -> foldl1' f $ listOrSingleton xs))),
  ("foldr", Modifier1 (\f -> dyadic (\x ys -> foldr' f x $ listOrSingleton ys))),
  ("foldr1", Modifier1 (\f -> monadic (\xs -> foldr1' f $ listOrSingleton xs))),
  ("invariant", Modifier1 (\f -> hook Builtin.fnSame f)),
  ("iterate", Modifier1 iterate'),
  ("lmap", Modifier1 mapLeft),
  ("map", Modifier1 mapZipping),
  ("not", Modifier1 (Builtin.fnNot <>)),
  ("rmap", Modifier1 mapRight),
  ("rotate", Modifier1 rotateArgs),
  ("scanl", Modifier1 (\f -> dyadic (\x ys -> List $ scanl' f x $ listOrSingleton ys))),
  ("scanl1", Modifier1 (\f -> monadic (\xs -> List $ scanl1' f $ listOrSingleton xs))),
  ("scanr", Modifier1 (\f -> dyadic (\x ys -> List $ scanr' f x $ listOrSingleton ys))),
  ("scanr1", Modifier1 (\f -> monadic (\xs -> List $ scanr1' f $ listOrSingleton xs))),
  ("self", Modifier1 $ convertArity 1),
  ("selftable", Modifier1 $ convertArity 1 . table),
  ("table", Modifier1 table),
  ("takewhile", Modifier1 (\f -> monadic (List . takeWhile' f . listOrSingleton))),
  ("zipwith", Modifier1 mapZipping),  -- Alias for map
  --- 2-modifiers ---
  ("and", Modifier2 (\f g -> ifThenElse f g f)),
  ("compose", Modifier2 compose2),
  ("hook", Modifier2 hook),
  ("or", Modifier2 (\f g -> ifThenElse f f g)),
  ("over", Modifier2 over),
  ("pair", Modifier2 (\f g -> hook (Builtin.fnPair <> f) g)),
  ("rcompose", Modifier2 rcompose2),
  --- 3-modifiers ---
  ("branch", Modifier3 (\f g h -> rcompose2 (f <> g) h)),
  ("compose3", Modifier3 compose3),
  ("fork", Modifier3 (\f g h -> hook (f <> g) h)),
  ("if", Modifier3 ifThenElse),
  ("rcompose3", Modifier3 rcompose3),
  --- 4-modifiers ---
  ("compose4", Modifier4 compose4)
  ]
