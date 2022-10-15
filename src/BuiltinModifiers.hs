module BuiltinModifiers (
  modifiers
) where

import Data.List (unfoldr)
import qualified Data.Map as Map
import Value (Value (..), listOrSingleton)
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

-- Pass different arguments to the second Function, then pass the results
-- as arguments to the first Function
over :: Function -> Function -> Function
over f g
  | arity f <= 1 = f <> g
  | otherwise = over' f g g

-- Modify a Function to take its first argument second and its second
-- argument first
-- If the original Function had arity 1, it becomes an arity-2 Function
-- that ignores its first argument
flipArgs :: Function -> Function
flipArgs f = Function (max 2 (arity f)) (\x -> bindSecond f x)

-- Modify a Function to take its last argument first
rotateArgs :: Function -> Function
rotateArgs f = Function (arity f) (\x -> rbind f x)

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
  | otherwise = Function a' $ collectArgs a' []
  where
    collectArgs a args arg
      | a == 1 = Constant $ applyFully f $ reverse (arg : args)
      | otherwise = Function (a - 1) $ collectArgs (a - 1) (arg : args)

-- Modify a Function to generate an infinite List by repeated appliction
-- The new Function has the same arity (call it N) as the original Function
-- The resulting List begins with the arguments unchanged; thereafter, each
-- element is the result of applying the Function over the preceding N
-- elements
iterate' :: Function -> Function
iterate' f = Function (arity f) (collectArgs (arity f) [])
  where
    collectArgs a args arg
      | a == 1 = Constant $ List $ unfoldr iterateStep $ reverse (arg : args)
      | otherwise = Function (a - 1) $ collectArgs (a - 1) (arg : args)
    iterateStep xs = Just (head xs, tail xs ++ [applyFully f xs])

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

-- The built-in modifiers are stored in a Map from names to Modifiers
modifiers :: Map.Map String Modifier
modifiers = Map.fromList [
  --- 1-modifiers ---
  ("flatmap", Modifier1 (\f -> Builtin.fnFlatten <> mapZipping f)),
  ("flip", Modifier1 flipArgs),
  ("invariant", Modifier1 (\f -> hook Builtin.fnSame f)),
  ("iterate", Modifier1 iterate'),
  ("lmap", Modifier1 mapLeft),
  ("map", Modifier1 mapZipping),
  ("not", Modifier1 (Builtin.fnNot <>)),
  ("rmap", Modifier1 mapRight),
  ("rotate", Modifier1 rotateArgs),
  ("self", Modifier1 $ convertArity 1),
  ("selftable", Modifier1 $ convertArity 1 . table),
  ("table", Modifier1 table),
  ("zipwith", Modifier1 mapZipping),  -- Alias for map
  --- 2-modifiers ---
  ("compose", Modifier2 compose2),
  ("hook", Modifier2 hook),
  ("over", Modifier2 over),
  ("pair", Modifier2 (\f g -> hook (Builtin.fnPair <> f) g)),
  ("rcompose", Modifier2 rcompose2),
  --- 3-modifiers ---
  ("branch", Modifier3 (\f g h -> rcompose2 (f <> g) h)),
  ("compose3", Modifier3 compose3),
  ("fork", Modifier3 (\f g h -> hook (f <> g) h)),
  ("rcompose3", Modifier3 rcompose3),
  --- 4-modifiers ---
  ("compose4", Modifier4 compose4)
  ]
