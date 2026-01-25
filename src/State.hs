{-# LANGUAGE NamedFieldPuns #-}

module State (
  executeProgram
) where

import Data.Maybe (listToMaybe)
import Value (Value)
import Function (ArgList, applyFully)
import StackOperation (applyOperation, pushFunction, applyModifier, bindValue)
import Stack (Stack)
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
applyCommand :: Command -> State -> State
-- Push a Function to the Stack
applyCommand (PushFn f) s = s{stack = stack'}
  where stack' = pushFunction (BF.implementation f) (stack s)
-- Apply a Modifier to the top elements of the Stack
applyCommand (ModifyFn m) s = s{stack = stack'}
  where stack' = applyModifier (BM.implementation m) (stack s)
-- Apply a StackOperation to the Stack
applyCommand (StackCmd cmd) s = s{stack = stack'}
  where stack' = applyOperation (BSO.implementation cmd) (stack s)
-- Bind a Value to the top element of the Stack
-- If the Value drops all the way through to the bottom of the Stack,
-- append it instead to the State's arguments list
applyCommand (BindVal x) s =
  case (bindValue x (stack s)) of
    Right stack' -> s{stack = stack'}
    Left x' -> s{stack = [], args = (args s) ++ [x']}
-- Bind the nth Value from the State's arguments list (using cyclical
-- indexing)
applyCommand (BindArg n) s
  | null (args s) = error $ "Cannot reference argument " ++ show n ++ ": the argument list is empty"
  | otherwise = applyCommand (BindVal $ indexCycle (args s) n) s
  where indexCycle l i = (cycle l) !! i

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
