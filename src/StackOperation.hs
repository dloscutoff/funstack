module StackOperation (
  StackOperation (..),
  pushFunction,
  applyModifier,
  bindValue
) where

import Stack (Stack, push, pop, popN)
import Value (Value)
import Function (Function (..), bind)
import Modifier (Modifier, degree, modify)

-- A StackOperation transforms a Stack into a new Stack
data StackOperation = StackOperation {
  arityIn :: Int,
  arityOut :: Int,
  applyOperation :: Stack -> Stack
}

-- Push a Function to a Stack, returning a new Stack
pushFunction :: Function -> Stack -> Stack
pushFunction = push

-- Apply a Modifer to a Stack, resulting in a new Stack
-- For an n-modifier, pop n Functions from the Stack, apply the modifier
-- function to them, and push the single resulting Function back onto
-- the Stack
applyModifier :: Modifier -> Stack -> Stack
applyModifier m s = push (modify m fs) s'
  where (fs, s') = popN (degree m) s

-- Bind a value to the top Function on the Stack:
--  If the result is a Function, push that Function back onto the Stack
--  If the result is a Constant, extract the Value and bind it to the
--   next Function on the Stack; if there are no Functions left on the
--   Stack, return the Value instead
bindValue :: Value -> Stack -> Either Value Stack
bindValue x [] = Left x
bindValue x s =
  case (bind f x) of
    Constant y -> bindValue y s'
    f' -> Right $ push f' s'
  where (f, s') = pop s
