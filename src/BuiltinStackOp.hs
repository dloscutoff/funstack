module BuiltinStackOp (
  BuiltinStackOp (..),
  implementation
) where

import StackOperation (StackOperation (..))
import State (Stack, pop)

-- Built-in stack operations are represented by the BuiltinStackOp type
data BuiltinStackOp =
  Dup |
  Swap |
  Tuck
  deriving (Show, Read)

-- Dup pops a Function and pushes it twice
opDup :: Stack -> Stack
opDup s = f : f : s'
  where (f, s') = pop s

-- Swap pops two Functions and pushes them in reverse order
opSwap :: Stack -> Stack
opSwap s = f : g : s''
  where
    (g, s') = pop s
    (f, s'') = pop s'

-- Tuck pops two Functions and pushes the top one, then the second
-- one, then the top one again
opTuck :: Stack -> Stack
opTuck s = g : f : g : s''
  where
    (g, s') = pop s
    (f, s'') = pop s'

-- Given a BuiltinStackOp, return the StackOperation that it represents
implementation :: BuiltinStackOp -> StackOperation
implementation o = case o of
  Dup -> StackOperation 1 2 opDup
  Swap -> StackOperation 2 2 opSwap
  Tuck -> StackOperation 2 3 opTuck
