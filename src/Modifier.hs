module Modifier (
  Modifier (..),
  modify
) where

import Function (Function)
import State (Stack, popN)

-- A Modifier transforms one or more Functions into a new Function
data Modifier =
  Modifier1 (Function -> Function) |
  Modifier2 (Function -> Function -> Function) |
  Modifier3 (Function -> Function -> Function -> Function) |
  Modifier4 (Function -> Function -> Function -> Function -> Function)

-- Modifiers are not normally shown, but can be in some error messages
instance Show Modifier where
  show (Modifier1 _) = "<1-modifier>"
  show (Modifier2 _) = "<2-modifier>"
  show (Modifier3 _) = "<3-modifier>"
  show (Modifier4 _) = "<4-modifier>"

-- Apply a Modifer to a Stack, resulting in a new Stack
-- For an n-modifier, pop n Functions from the Stack, apply the modifier
-- function to them, and push the single resulting Function back onto
-- the Stack
modify :: Stack -> Modifier -> Stack
modify s m
  | (Modifier1 mfn) <- m, ([f], s') <- popN 1 s = mfn f : s'
  | (Modifier2 mfn) <- m, ([f, g], s') <- popN 2 s = mfn f g : s'
  | (Modifier3 mfn) <- m, ([f, g, h], s') <- popN 3 s = mfn f g h : s'
  | (Modifier4 mfn) <- m, ([f, g, h, i], s') <- popN 4 s = mfn f g h i : s'
  | otherwise = error $ "Error while applying " ++ show m ++ " to stack " ++ show s
