module BuiltinStackOp (
  BuiltinStackOp (..),
  implementation
) where

import Data.Function ((&))
import StackOperation (StackOperation (..))
import Stack (Stack, push, pop)
import Box (Box (..))

-- Built-in stack operations are represented by the BuiltinStackOp type
data BuiltinStackOp =
  Box |
  Drop |
  Dup |
  Over |
  Rot |
  Swap |
  Tuck |
  Unbox
  deriving (Show, Read)

-- Box pops two items, puts them in a Box, and pushes it
opBox :: Stack -> Stack
opBox s = s'' & push b
  where
    b = Multiple [f, g]
    (f, s'') = pop s'
    (g, s') = pop s

-- Drop pops an item and discards it
opDrop :: Stack -> Stack
opDrop s = s'
  where (_, s') = pop s

-- Dup pops an item and pushes it twice
opDup :: Stack -> Stack
opDup s = s' & push f & push f
  where (f, s') = pop s

-- Over pops two items and pushes the second one, then the top
-- one, then the second one again
opOver :: Stack -> Stack
opOver s = s'' & push f & push g & push f
  where
    (f, s'') = pop s'
    (g, s') = pop s

-- Rot pops three items and pushes the second one, then the top
-- one, then the third one
opRot :: Stack -> Stack
opRot s = s''' & push g & push h & push f
  where
    (f, s''') = pop s''
    (g, s'') = pop s'
    (h, s') = pop s

-- Swap pops two items and pushes them in reverse order
opSwap :: Stack -> Stack
opSwap s = s'' & push g & push f
  where
    (f, s'') = pop s'
    (g, s') = pop s

-- Tuck pops two items and pushes the top one, then the second
-- one, then the top one again
opTuck :: Stack -> Stack
opTuck s = s'' & push g & push f & push g
  where
    (f, s'') = pop s'
    (g, s') = pop s

-- Unbox pops a Box:
--  If it's a Multiple, extract and push its contents
--  If it's a Single, push it unchanged
opUnbox :: Stack -> Stack
opUnbox s =
  case b of
    Multiple bs -> foldl (flip push) s' bs
    Single _ -> s' & push b
  where (b, s') = pop s

-- Given a BuiltinStackOp, return the StackOperation that it represents
implementation :: BuiltinStackOp -> StackOperation
implementation o = case o of
  Box -> StackOperation opBox
  Drop -> StackOperation opDrop
  Dup -> StackOperation opDup
  Over -> StackOperation opOver
  Rot -> StackOperation opRot
  Swap -> StackOperation opSwap
  Tuck -> StackOperation opTuck
  Unbox -> StackOperation opUnbox
