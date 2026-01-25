module Command (
  Command (..)
) where

import Value (Value)
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
