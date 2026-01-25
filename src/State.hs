module State (
  Stack,
  State (..),
  emptyState,
  push,
  pop,
  popN
) where

import Function (Function, ArgList)

-- A Stack is simply a list of Functions
type Stack = [Function]

-- State represents an intermediate stage in the process of creating a
-- complex Function and applying it to a list of argument Values
data State = State {
  stack :: Stack,
  args :: ArgList
} deriving (Show)

-- Create a new State with the given arguments and an empty stack
emptyState :: ArgList -> State
emptyState xs = State{stack = [], args = xs}

-- Push a Function to a Stack, returning a new Stack
push :: Function -> Stack -> Stack
push = (:)

-- Pop the top Function from a Stack, returning the Function and a
-- new Stack with it missing
-- If the Stack is empty, use the identity function
pop :: Stack -> (Function, Stack)
pop (f : fs) = (f, fs)
pop [] = (mempty, [])

-- Pop n Functions from a Stack, returning a list of Functions and a
-- new Stack with them missing
-- The Functions are listed with the top of the Stack at the end of
-- the list, in keeping with the order in which they were (probably)
-- pushed: a program like A B over should give A over B, not B over A
-- If the Stack does not contain enough elements, fill in with the
-- identity function
popN :: Int -> Stack -> ([Function], Stack)
popN n s = (reverse $ take n $ s ++ cycle [mempty], drop n s)
