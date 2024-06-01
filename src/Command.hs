{-# LANGUAGE NamedFieldPuns #-}

module Command (
  Command (..),
  executeProgram
) where

import Data.Maybe (listToMaybe)
import Value (Value)
import Function (Function (..), bind, applyFully)
import Modifier (modify)
import StackOperation (transform)
import State (State (..), emptyState)
import qualified BuiltinFunction as BF
import qualified BuiltinModifier as BM
import qualified BuiltinStackOp as BSO

-- A Command represents the change to the program state caused by a single
-- token of the program
--  PushFn: Push a built-in Function to the Stack
--  ModifyFn: Modify the top Function(s) on the Stack
--  StackCmd: Modify the Stack in some way
--  BindVal: Bind a Value to the topmost Function on the Stack
--  BindArg: Bind one of the argument values to the topmost Function
data Command =
  PushFn BF.BuiltinFunction |
  ModifyFn BM.BuiltinModifier |
  StackCmd BSO.BuiltinStackOp |
  BindVal Value |
  BindArg Int

-- Apply a Command to a State, returning an updated State
-- BindArg binds the nth element of the current arguments list (using
-- cyclical indexing)
-- BindVal binds its value to the top Function:
--  If the result is a Function, it pushes that Function back onto the Stack
--  If the result is a Constant, it extracts the Value and binds it to the
--   next Function on the Stack
-- If there are no Functions left on the Stack, BindVal instead appends its
-- Value to the arguments list
executeCommand :: State -> Command -> State
executeCommand state@State{stack} (PushFn f) =
  state{stack = BF.implementation f : stack}
executeCommand state@State{stack} (ModifyFn m) =
  state{stack = modify (BM.implementation m) stack}
executeCommand state@State{stack} (StackCmd cmd) =
  state{stack = transform (BSO.implementation cmd) stack}
executeCommand state@State{arguments} (BindArg n) =
  executeCommand state (BindVal $ indexCycle arguments n)
  where indexCycle l i = (cycle l) !! i
executeCommand state@State{stack = [], arguments} (BindVal x) =
  state{arguments = arguments ++ [x]}
executeCommand state@State{stack = f : fs} (BindVal x) =
  case (bind f x) of
    Constant y -> executeCommand state{stack = fs} (BindVal y)
    f' -> state{stack = f' : fs}

-- Apply a line of commands to an initially empty State, then apply the
-- function represented by the final Stack to the arguments to get a
-- return Value
--  If the final State has no arguments, no Value can be returned
--  If the final State has no Functions on the Stack, return the last Value
--   in the arguments list (to which at least one new value has likely been
--   added during the execution of the line)
--  Otherwise, compose the Functions on the Stack together, apply the
--   combined function to the arguments, and return the result
executeLine :: [Command] -> [Value] -> Maybe Value
executeLine commands args
  | null arguments = Nothing
  | null stack = Just $ last arguments
  | otherwise = Just $ applyFully (composeAll stack) arguments
  where
    composeAll = mconcat . reverse
    State{stack, arguments} =
      foldl executeCommand (emptyState args) commands

-- Execute the first line of the program as the main function
-- If the program is empty, return the first argument (if any)
-- TODO: subsequent lines as helper functions?
executeProgram :: [[Command]] -> [Value] -> Maybe Value
executeProgram (firstLine : _) = executeLine firstLine
executeProgram [] = listToMaybe
