module Modifier (
  Modifier (..),
  modify
) where

import Function (Function)
import State (Stack)

-- A Modifier transforms one or more Functions into a new Function
data Modifier =
  Modifier1 (Function -> Function) |
  Modifier2 (Function -> Function -> Function) |
  Modifier3 (Function -> Function -> Function -> Function) |
  Modifier4 (Function -> Function -> Function -> Function -> Function)

-- Apply a Modifer to a Stack, resulting in a new Stack
-- For an n-modifier, pop n Functions from the Stack, apply the modifier
-- function to them in the reverse of the order in which they were popped,
-- and push the single resulting Function back onto the Stack
-- If there are not enough Functions on the Stack to apply the Modifier,
-- push copies of the identity Function until there are enough
modify :: Stack -> Modifier -> Stack
modify (f : fs) (Modifier1 m) = m f : fs
modify (g : f : fs) (Modifier2 m) = m f g : fs
modify (h : g : f : fs) (Modifier3 m) = m f g h : fs
modify (i : h : g : f : fs) (Modifier4 m) = m f g h i : fs
modify fs m = modify (mempty : fs) m
