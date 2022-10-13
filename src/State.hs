module State (
  Stack,
  State (..),
  emptyState
) where

import Value (Value)
import Function (Function)

-- A Stack is simply a list of Functions
type Stack = [Function]

-- State represents an intermediate stage in the process of creating a
-- complex Function and applying it to a list of argument Values
data State = State {
  stack :: Stack,
  arguments :: [Value]
} deriving (Show)

-- Create a new State with the given arguments and an empty stack
emptyState :: [Value] -> State
emptyState args = State [] args
