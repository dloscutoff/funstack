module StackOperation (
  StackOperation (..)
) where

import Stack (Stack)

-- A StackOperation transforms a Stack into a new Stack
data StackOperation = StackOperation {
  arityIn :: Int,
  arityOut :: Int,
  transform :: Stack -> Stack
}
