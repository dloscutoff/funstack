module Stack (
  Stack,
  Box (..),
  push,
  pop,
  popN
) where

import Function (Function)
import Box (Box (..))

-- A Stack is a list of Boxes of Functions
type Stack = [Box Function]

-- Push a Box to a Stack, returning a new Stack
push :: Box Function -> Stack -> Stack
push = (:)

-- Pop the top Box from a Stack, returning the Box and a new Stack
-- with it missing
-- If the Stack is empty, use a Box containing the identity function
pop :: Stack -> (Box Function, Stack)
pop (b : bs) = (b, bs)
pop [] = (Single mempty, [])

-- Pop n Boxes from a Stack, returning a list of Boxes and a new Stack
-- with them missing
-- The Boxes are returned with the top of the Stack at the end of
-- the list, in keeping with the order in which they were (probably)
-- pushed: a program like A B over should give A over B, not B over A
-- If the Stack does not contain enough elements, fill in with the
-- identity function
popN :: Int -> Stack -> ([Box Function], Stack)
popN n s = (reverse $ take n s', drop n s)
  where s' = s ++ cycle [Single mempty]
