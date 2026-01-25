module BuiltinStackOp (
  BuiltinStackOp (..),
  implementation
) where

import Data.Function ((&))
import StackOperation (StackOperation (..))
import State (Stack, push, pop)

-- Built-in stack operations are represented by the BuiltinStackOp type
data BuiltinStackOp =
  Drop |
  Dup |
  Over |
  Rot |
  Swap |
  Tuck
  deriving (Show, Read)

-- Drop pops a Function
opDrop :: Stack -> Stack
opDrop s = s'
  where (_, s') = pop s

-- Dup pops a Function and pushes it twice
opDup :: Stack -> Stack
opDup s = s' & push f & push f
  where (f, s') = pop s

-- Over pops two Functions and pushes the second one, then the top
-- one, then the second one again
opOver :: Stack -> Stack
opOver s = s'' & push f & push g & push f
  where
    (f, s'') = pop s'
    (g, s') = pop s

-- Rot pops three Functions and pushes the second one, then the top
-- one, then the third one
opRot :: Stack -> Stack
opRot s = s''' & push g & push h & push f
  where
    (f, s''') = pop s''
    (g, s'') = pop s'
    (h, s') = pop s

-- Swap pops two Functions and pushes them in reverse order
opSwap :: Stack -> Stack
opSwap s = s'' & push g & push f
  where
    (f, s'') = pop s'
    (g, s') = pop s

-- Tuck pops two Functions and pushes the top one, then the second
-- one, then the top one again
opTuck :: Stack -> Stack
opTuck s = s'' & push g & push f & push g
  where
    (f, s'') = pop s'
    (g, s') = pop s

-- Given a BuiltinStackOp, return the StackOperation that it represents
implementation :: BuiltinStackOp -> StackOperation
implementation o = case o of
  Drop -> StackOperation 1 0 opDrop
  Dup -> StackOperation 1 2 opDup
  Over -> StackOperation 2 3 opOver
  Rot -> StackOperation 3 3 opRot
  Swap -> StackOperation 2 2 opSwap
  Tuck -> StackOperation 2 3 opTuck
