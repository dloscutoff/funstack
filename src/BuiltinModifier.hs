module BuiltinModifier (
  BuiltinModifier (..),
  rcompose2,
  implementation
) where

import Data.List (unfoldr)
import Value (Value (..), fromValue, toValue)
import Function (
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
  dyadic
  )
import Modifier (Modifier (..))
import BuiltinFunction (windows)
import qualified BuiltinFunction as BF

-- Built-in modifiers are represented by the BuiltinModifier type
data BuiltinModifier =
  And |
  Arity2 |
  Arity3 |
  Branch |
  Compose |
  Compose3 |
  Compose4 |
  Dropwhile |
  Filter |
  Fixiter |
  Fixpoint |
  Flatmap |
  Flip |
  Foldl |
  Foldl1 |
  Foldr |
  Foldr1 |
  Fork |
  Hook |
  If |
  Invariant |
  Iterate |
  Lmap |
  Mapwindows |
  Mapzip |
  Not |
  Or |
  Over |
  Pair |
  Rcompose |
  Rcompose3 |
  Rmap |
  Rotate |
  Scanl |
  Scanl1 |
  Scanr |
  Scanr1 |
  Self |
  Selftable |
  Table |
  Takewhile |
  Treeapply |
  Treemapzip |
  Treeunfold |
  Treewalk |
  Unfoldr |
  Until |
  While
  deriving (Show, Read)

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
      | a' == 1 = Function 1 (\x -> if (fromValue $ apply f' x) then (bind g' x) else (bind h' x))
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

-- Given a function Arity, return a new Function that works as follows:
--  Collect that many arguments in a list
--  Once the full number of arguments has been collected, apply some Haskell
--  function to the collected argument list and return a Value
collectArgs :: Arity -> (ArgList -> Value) -> Function
collectArgs a applyFn = Function a $ collectArgs' a []
  where
    collectArgs' a' args arg
      | a' <= 1 = Constant $ applyFn $ reverse (arg : args)
      | otherwise = Function (a' - 1) $ collectArgs' (a' - 1) (arg : args)

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
  | otherwise = collectArgs a' (applyFully f)

-- Modify a Function to generate an infinite ValList by repeated appliction
-- The new Function has the same arity (call it N) as the original Function
-- The resulting ValList begins with the arguments unchanged; thereafter, each
-- element is the result of applying the Function over the preceding N
-- elements
iterate' :: Function -> Function
iterate' f = collectArgs (arity f) (toValue . unfoldr iterateStep)
  where
    iterateStep args = Just (head args, tail args ++ [newVal])
      where newVal = applyFully f args

-- Same as iterate', but stop when the results stop changing
-- For higher-arity Functions, the entire argument list must remain unchanged
-- to stop the iteration
fixiter :: Function -> Function
fixiter f = collectArgs (arity f) (toValue . fixiterApply)
  where
    fixiterApply args
      | all (== newVal) args = args
      | otherwise = head args : fixiterApply (tail args ++ [newVal])
      where newVal = applyFully f args

-- Same as fixiter, but only return the final result, not the whole list
fixpoint :: Function -> Function
fixpoint f = collectArgs (arity f) fixpointApply
  where
    fixpointApply args
      | all (== newVal) args = newVal
      | otherwise = fixpointApply (tail args ++ [newVal])
      where newVal = applyFully f args

-- Given a Function and a list of Values, take elements from the list while
-- applying the Function to them results in a truthy Value
-- For higher-arity Functions, include elements up to but not including
-- the first element that makes the predicate falsey
takeWhile' :: Function -> [Value] -> [Value]
takeWhile' f xs
  | length args < arity f = xs
  | fromValue (applyFully f args) = head xs : takeWhile' f (tail xs)
  | otherwise = init args
  where args = take (arity f) xs

-- Given a Function and a list of Values, drop elements from the list while
-- applying the Function to them results in a truthy Value
-- For higher-arity Functions, discard elements up to but not including
-- the first element that makes the predicate falsey
dropWhile' :: Function -> [Value] -> [Value]
dropWhile' f xs
  | length args < arity f = []
  | fromValue (applyFully f args) = dropWhile' f (tail xs)
  | otherwise = drop (arity f - 1) xs
  where args = take (arity f) xs

-- Given a Function and a list of Values, keep only elements from the list
-- where applying the Function to them results in a truthy Value
-- TODO: Is there a sensible definition for higher-arity Functions?
filter' :: Function -> [Value] -> [Value]
filter' f l
  | arity f > 1 = error "Function for filter must have arity 1"
  | otherwise = filter (fromValue . apply f) l

-- Bind multiple Functions to an argument and return a list of new Functions
parallelBind :: [Function] -> Value -> [Function]
parallelBind fs x = [bind f x | f <- fs]

-- Bind multiple Functions to an argument and return a list of new Functions
-- If the argument is a ValList, zip the Functions with the elements of the
-- argument; otherwise, bind each Function to the argument
zipBind :: [Function] -> Value -> [Function]
zipBind fs (ValList l) = zipWith bind fs l
zipBind fs x = parallelBind fs x

-- Apply multiple arity-1 Functions to an argument and return a ValList of
-- the results
parallelApply :: [Function] -> Value -> Value
parallelApply fs x = toValue [apply f x | f <- fs]

-- Apply multiple arity-1 Functions to an argument and return a ValList of
-- the results; if the argument is a ValList, zip the Functions with the
-- elements of the argument; otherwise, apply each Function to the argument
zipApply :: [Function] -> Value -> Value
zipApply fs (ValList l) = toValue $ zipWith apply fs l
zipApply fs x = parallelApply fs x

-- Given an arity and a list of Functions of that arity, turn them into a
-- single Function that applies each of the constituent Functions in parallel
-- and returns a ValList of the results
parallel :: Arity -> [Function] -> Function
parallel a fs
  | a < 1 = error $ "Cannot call parallel with arity " ++ show a ++ " less than 1"
  | a == 1 = Function 1 (Constant . parallelApply fs)
  | otherwise = Function a (parallel (a - 1) . parallelBind fs)

-- Given an arity and a list of Functions of that arity, turn them into a
-- single Function that applies each of the constituent Functions in parallel
-- and returns a ValList of the results
zipParallel :: Arity -> [Function] -> Function
zipParallel a fs
  | a < 1 = error $ "Cannot call zipParallel with arity " ++ show a ++ " less than 1"
  | a == 1 = Function 1 (Constant . zipApply fs)
  | otherwise = Function a (zipParallel (a - 1) . zipBind fs)

-- Convert a Function to map over its arguments
-- If the Function takes multiple arguments, zip the argument lists
-- together (truncating to the length of the shortest one)
-- However, if an argument is not a ValList, it is used across the board
-- If all arguments are non-ValLists, the result is a singleton ValList
mapZipping :: Function -> Function
mapZipping f
  | arity f == 1 =
      monadic (\xs -> toValue [apply f x | x <- fromValue xs])
  | otherwise = Function (arity f) mapBind
  where
    mapBind (ValList l) = zipParallel (arity f - 1) (map (bind f) l)
    mapBind x = mapZipping $ bind f x

-- Convert a Function to map recursively over its list arguments (trees)
-- and apply directly to its non-list arguments (leaves)
-- If the Function takes multiple arguments, zip the argument lists
-- together (truncating to the length of the shortest one)
-- However, if an argument is not a ValList, it is used across the board
treeMapZipping :: Function -> Function
treeMapZipping f
  | arity f == 1 = monadic treeMapApply
  | otherwise = Function (arity f) treeMapBind
  where
    treeMapBind (ValList l) = zipParallel (arity f - 1) (map (bind $ treeMapZipping f) l)
    treeMapBind x = treeMapZipping $ bind f x
    treeMapApply (ValList l) = toValue [apply (treeMapZipping f) x | x <- l]
    treeMapApply x = apply f x

-- Convert a Function to apply recursively over a nested ValList, interpreted
-- as a tree
-- Apply the Function to each level of the ValList after recursing
-- Leave non-ValLists (leaves) unchanged
treeApply :: Function -> Value -> Value
treeApply f t
  | arity f > 1 = error "Function for treeapply must have arity 1"
  | (ValList l) <- t = apply f $ toValue $ map (treeApply f) l
  | leaf <- t = leaf

-- Convert three Functions to walk recursively over a nested ValList, interpreted
-- as a tree, applying the Functions:
--  - Apply the first Function to each level of the ValList before recursing
--  - Apply the second Function to non-ValLists (leaf nodes)
--  - Apply the third Function to each level of the ValList after recursing
treeWalk :: Function -> Function -> Function -> Value -> Value
treeWalk f g h t
  | maximum (map arity [f, g, h]) > 1 = error "Functions for treewalk must have arity 1"
  | (ValList _) <- t = treeWalk' $ apply f t
  | otherwise = treeWalk' t
  where
    treeWalk' (ValList l) = apply h $ toValue $ map (treeWalk f g h) l
    treeWalk' leaf = apply g leaf

-- Convert a Function to generate a nested list, interpreted as a tree,
-- by applying it to a seed Value:
--  - If the result is a ValList, recurse over the elements of the ValList
--  - If not, return the result unchanged (leaf node)
treeUnfold :: Function -> Value -> Value
treeUnfold f t
  | arity f > 1 = error "Function for treeunfold must have arity 1"
  | otherwise =
    case (apply f t) of
      (ValList l) -> toValue $ map (treeUnfold f) l
      leaf -> leaf

-- Convert a Function to map over its first argument
-- If the Function only takes one argument, apply it to each element of its
-- argument, pair the result with the original element, and return a ValList
-- of those pairs
-- If the first argument is not a ValList, wrap it in a singleton ValList
mapLeft :: Function -> Function
mapLeft f
  | a == 1 =
      monadic (\xs -> toValue [[apply f x, x] | x <- fromValue xs])
  | otherwise =
      Function a (\xs -> parallel (a - 1) [bind f x | x <- fromValue xs])
  where a = arity f

-- Convert a Function to map over its last argument
-- If the Function only takes one argument, pair each element of its original
-- argument with the result of applying the function to that element, and 
-- return a ValList of those pairs
-- If the first argument is not a ValList, wrap it in a singleton ValList
mapRight :: Function -> Function
mapRight f
  | a == 1 =
      monadic (\xs -> toValue [[x, apply f x] | x <- fromValue xs])
  | a == 2 =
      dyadic (\x ys -> toValue [apply2 f x y | y <- fromValue ys])
  | otherwise = Function a (\x -> mapRight $ bind f x)
  where a = arity f

-- Convert an arity-N Function to map over size-N windows from a ValList
mapWindows :: Function -> Function
mapWindows f = monadic (\xs -> toValue $ map (applyFully f) $ windows (arity f) (fromValue xs))

-- Convert an arity-N Function to a Function that creates a depth-N nested
-- ValList containing the results of applying the original Function to every
-- combination of values from its arguments (N-dimensional outer product)
table :: Function -> Function
table f
  | a < 1 = f
  | a == 1 = mapZipping f
  | otherwise =
      Function a (\xs -> parallel (a - 1) [table $ bind f x | x <- fromValue xs])
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

-- Given two Functions and an initial argument list of Values, perform
-- a generalized right unfold
-- At each step, the first Function is applied to the arglist and that
-- result is appended to the result list
-- Then the second Function is applied to the arglist; that result is
-- appended to the arglist, and the first element of the arglist is removed
-- Once the first element of the arglist is falsey, the process stops
unfoldr' :: Function -> Function -> ArgList -> [Value]
unfoldr' f g args
  | null args = error "Cannot unfoldr' empty arglist"
  | fromValue (head args) = applyFully f args : unfoldr' f g (tail args ++ [applyFully g args])
  | otherwise = []

-- Given a Function, apply the takewhile modifier to it and return a
-- new Function
modTakeWhile :: Function -> Function
modTakeWhile f = monadic $ toValue . takeWhile' f . fromValue

-- Given two Functions, apply the unfoldr modifier to them and return a
-- new Function
-- The arity of the combined Function is the max of the arities of
-- the two Functions
modUnfoldR :: Function -> Function -> Function
modUnfoldR f g = collectArgs (arity f `max` arity g) (toValue . unfoldr' f g)

-- Given a BuiltinModifier, return the Modifier that it represents
implementation :: BuiltinModifier -> Modifier
implementation m = case m of
  --- 1-modifiers ---
  Arity2 -> Modifier1 $ convertArity 2
  Arity3 -> Modifier1 $ convertArity 3
  Dropwhile -> Modifier1 (\f -> monadic (toValue . dropWhile' f . fromValue))
  Filter -> Modifier1 (\f -> monadic (toValue . filter' f . fromValue))
  Fixiter -> Modifier1 fixiter
  Fixpoint -> Modifier1 fixpoint
  Flatmap -> Modifier1 (\f -> BF.fnFlatten <> mapZipping f)
  Flip -> Modifier1 flipArgs
  Foldl -> Modifier1 (\f -> dyadic (\x ys -> foldl' f x $ fromValue ys))
  Foldl1 -> Modifier1 (\f -> monadic (\xs -> foldl1' f $ fromValue xs))
  Foldr -> Modifier1 (\f -> dyadic (\x ys -> foldr' f x $ fromValue ys))
  Foldr1 -> Modifier1 (\f -> monadic (\xs -> foldr1' f $ fromValue xs))
  Invariant -> Modifier1 (\f -> hook BF.fnSame f)
  Iterate -> Modifier1 iterate'
  Lmap -> Modifier1 mapLeft
  Mapzip -> Modifier1 mapZipping
  Mapwindows -> Modifier1 mapWindows
  Not -> Modifier1 (BF.fnNot <>)
  Rmap -> Modifier1 mapRight
  Rotate -> Modifier1 rotateArgs
  Scanl -> Modifier1 (\f -> dyadic (\x ys -> toValue $ scanl' f x $ fromValue ys))
  Scanl1 -> Modifier1 (\f -> monadic (\xs -> toValue $ scanl1' f $ fromValue xs))
  Scanr -> Modifier1 (\f -> dyadic (\x ys -> toValue $ scanr' f x $ fromValue ys))
  Scanr1 -> Modifier1 (\f -> monadic (\xs -> toValue $ scanr1' f $ fromValue xs))
  Self -> Modifier1 $ convertArity 1
  Selftable -> Modifier1 $ convertArity 1 . table
  Table -> Modifier1 table
  Takewhile -> Modifier1 modTakeWhile
  Treeapply -> Modifier1 (monadic . treeApply)
  Treemapzip -> Modifier1 treeMapZipping
  Treeunfold -> Modifier1 (monadic . treeUnfold)
  --- 2-modifiers ---
  And -> Modifier2 (\f g -> ifThenElse f g f)
  Compose -> Modifier2 compose2
  Hook -> Modifier2 hook
  Or -> Modifier2 (\f g -> ifThenElse f f g)
  Over -> Modifier2 over
  Pair -> Modifier2 (\f g -> hook (BF.fnPair <> f) g)
  Rcompose -> Modifier2 rcompose2
  Unfoldr -> Modifier2 modUnfoldR
  Until -> Modifier2 (\f g -> modTakeWhile (BF.fnNot <> f) <> iterate' g)
  While -> Modifier2 (\f g -> modTakeWhile f <> iterate' g)
  --- 3-modifiers ---
  Branch -> Modifier3 (\f g h -> rcompose2 (f <> g) h)
  Compose3 -> Modifier3 compose3
  Fork -> Modifier3 (\f g h -> hook (f <> g) h)
  If -> Modifier3 ifThenElse
  Rcompose3 -> Modifier3 rcompose3
  Treewalk -> Modifier3 (\f g h -> monadic $ treeWalk f g h)
  --- 4-modifiers ---
  Compose4 -> Modifier4 compose4
