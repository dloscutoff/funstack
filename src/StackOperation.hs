module StackOperation (
  StackOperation (..),
  pushFunction,
  applyModifier,
  bindValue
) where

import Stack (Stack, push, pop, popN)
import Box (Box (..))
import Value (Value)
import Function (Function (..), bind)
import Modifier (Modifier, degree, modify)

-- A StackOperation transforms a Stack into a new Stack
data StackOperation = StackOperation {
  applyOperation :: Stack -> Stack
}

-- Wrap a Function in a Box and push it to a Stack, returning a new Stack
pushFunction :: Function -> Stack -> Stack
pushFunction f = push (Single f)

-- Apply a Modifer to a Stack, resulting in a new Stack
-- For an n-modifier, pop n Boxes from the Stack, apply the modifier
-- function to their contents in parallel, and push the single resulting
-- Box back onto the Stack
applyModifier :: Modifier -> Stack -> Stack
applyModifier m s = push (modify m <$> sequenceA bs) s'
  where (bs, s') = popN (degree m) s

-- Bind a value to the top Box on the Stack:
--  If the result is a Single Function, push it back onto the Stack
--  If the result is a Single Constant, extract the Value and bind it to
--   the next Box on the Stack; if there are no Boxes left on the Stack,
--   return the Value instead
bindValue :: Value -> Stack -> Either Value Stack
bindValue x [] = Left x
bindValue x s =
  case (flip bind x <$> b) of
    Single (Constant y) -> bindValue y s'
    b' -> Right $ push b' s'
  where (b, s') = pop s
