module Modifier (
  Modifier (..),
  degree,
  modify
) where

import Function (Function)

-- A Modifier transforms one or more Functions into a new Function
data Modifier =
  Modifier1 (Function -> Function) |
  Modifier2 (Function -> Function -> Function) |
  Modifier3 (Function -> Function -> Function -> Function) |
  Modifier4 (Function -> Function -> Function -> Function -> Function)

-- Return the degree of a Modifier, i.e. the number of Functions it combines
degree :: Modifier -> Int
degree Modifier1{} = 1
degree Modifier2{} = 2
degree Modifier3{} = 3
degree Modifier4{} = 4

-- Modifiers are not normally shown, but can be in some error messages
instance Show Modifier where
  show (Modifier1 _) = "<1-modifier>"
  show (Modifier2 _) = "<2-modifier>"
  show (Modifier3 _) = "<3-modifier>"
  show (Modifier4 _) = "<4-modifier>"

-- Apply a Modifier to a list of Functions and return the resulting Function
modify :: Modifier -> [Function] -> Function
modify (Modifier1 m) [f] = m f
modify (Modifier2 m) [f, g] = m f g
modify (Modifier3 m) [f, g, h] = m f g h
modify (Modifier4 m) [f, g, h, i] = m f g h i
modify m fs = error $ "Error while applying " ++ show m ++ " to functions " ++ show fs
