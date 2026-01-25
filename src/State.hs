{-# LANGUAGE NamedFieldPuns #-}

module State (
  executeProgram
) where

import Data.Maybe (listToMaybe)
import Value (Value)
import Function (Function (..), ArgList, bind, applyFully)
import Modifier (modify)
import StackOperation (transform)
import Stack (Stack, push, pop)
import Command (Command (..))
import qualified BuiltinFunction as BF
import qualified BuiltinModifier as BM
import qualified BuiltinStackOp as BSO

-- State represents an intermediate stage in the process of creating a
-- complex Function and applying it to a list of argument Values
data State = State {
  stack :: Stack,
  args :: ArgList
} deriving (Show)

-- Create a new State with the given arguments and an empty stack
emptyState :: ArgList -> State
emptyState xs = State{stack = [], args = xs}

-- Apply a Command to a State, returning an updated State
-- BindArg binds the nth element of the current arguments list (using
-- cyclical indexing)
-- BindVal binds its value to the top Function:
--  If the result is a Function, it pushes that Function back onto the Stack
--  If the result is a Constant, it extracts the Value and binds it to the
--   next Function on the Stack
-- If there are no Functions left on the Stack, BindVal instead appends its
-- Value to the arguments list
applyCommand :: Command -> State -> State
applyCommand (PushFn f) state@State{stack} =
  state{stack = push (BF.implementation f) stack}
applyCommand (ModifyFn m) state@State{stack} =
  state{stack = modify (BM.implementation m) stack}
applyCommand (StackCmd cmd) state@State{stack} =
  state{stack = transform (BSO.implementation cmd) stack}
applyCommand (BindArg n) state@State{args} =
  applyCommand (BindVal $ indexCycle args n) state
  where indexCycle l i = (cycle l) !! i
applyCommand (BindVal x) state@State{stack = [], args} =
  state{args = args ++ [x]}
applyCommand (BindVal x) state@State{stack} =
  case (bind top x) of
    Constant y -> applyCommand (BindVal y) state{stack = stack'}
    top' -> state{stack = push top' stack'}
  where (top, stack') = pop stack

-- Apply a list of Commands to an initially empty State, returning the
-- final State
applyCommands :: [Command] -> ArgList -> State
applyCommands cmds args = foldl (flip applyCommand) (emptyState args) cmds

-- Given a State, compose the Stack into one Function and apply it
-- to the ArgList to get a return Value
--  If the State has an empty ArgList, no Value can be returned
--  If the State has an empty Stack, return the last Value in the ArgList
--   (to which at least one new value has likely been added during the
--   execution of the line)
applyStack :: State -> Maybe Value
applyStack State{args = []} = Nothing
applyStack State{stack = [], args} = Just $ last args
applyStack State{stack, args} = Just $ applyFully (composeAll stack) args
  where composeAll = mconcat . reverse

-- Execute a line of Commands on an initially empty State and return
-- the resulting Value
executeLine :: [Command] -> ArgList -> Maybe Value
executeLine cmds = applyStack . applyCommands cmds

-- Execute the first line of the program as the main function
-- If the program is empty, return the first argument (if any)
-- TODO: subsequent lines as helper functions?
executeProgram :: [[Command]] -> ArgList -> Maybe Value
executeProgram (firstLine : _) = executeLine firstLine
executeProgram [] = listToMaybe
