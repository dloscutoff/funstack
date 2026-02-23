module Interpolation (
  Interpolation,
  InterpolationComponent (..),
  condense,
  function
) where

import Text.Read (readPrec, readMaybe)
import Text.ParserCombinators.ReadPrec (get, pfail)
import Value (Value (..), stringToVal)
import Function (Function (..), bind)
import BuiltinFunction (fnConcat, fnStringify)
import BuiltinModifier (rcompose2)

data InterpolationComponent =
  LiteralString String |
  Interpolate
  deriving (Show)

instance Read InterpolationComponent where
  readPrec = do
    c <- get
    case c of
      '$' -> pure Interpolate
      '\\' -> do
        d <- get
        case readMaybe ('\'' : c : d : "'") of
          Just c' -> pure $ LiteralString (c' : "")
          Nothing -> if d `elem` "$`\\" then pure $ LiteralString (d : "") else pfail
      _ -> pure $ LiteralString (c : "")

type Interpolation = [InterpolationComponent]

-- Condense an Interpolation by concatenating successive LiteralStrings
condense :: Interpolation -> Interpolation
condense (ic : ias)
  | (LiteralString s) <- ic, (LiteralString s' : i') <- i = LiteralString (s ++ s') : i'
  | otherwise = ic : i
  where i = condense ias
condense [] = []

-- Convert an Interpolation to a Function
-- The arity is dependent on how many interpolations there are
function :: Interpolation -> Function
function (ic : ics)
  | Interpolate <- ic = rcompose2 (fnConcat <> fnStringify) f'
  | LiteralString s <- ic = bind fnConcat (stringToVal s) <> f'
  where
    f' = function ics
function [] = Constant $ ValList []
