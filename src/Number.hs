module Number (
  Number (..),
  numerator,
  denominator,
  toNumber,
  naturals
) where

import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec (get, (<++))
import qualified Data.Ratio as Ratio

data Number = Integer :% Integer deriving (Eq)

numerator :: Number -> Integer
numerator (a :% _) = a

denominator :: Number -> Integer
denominator (_ :% b) = b

reduce :: Integer -> Integer -> Number
reduce 0 0 = error "Dividing 0 by 0 is undefined"
reduce a b
  | b >= 0, g <- gcd a b = (a `div` g) :% (b `div` g)
  | otherwise = reduce (-a) (-b)

instance Show Number where
  show (a :% 1) = show a
  show (a :% b) = show a ++ "/" ++ show b

instance Read Number where
  -- Read either a ratio of two Integers or a single Integer
  readPrec = readRatio <++ readInt
    where
      readRatio = do
        a <- readPrec
        '/' <- get
        b <- readPrec
        pure (reduce a b)
      readInt = do
        a <- readPrec
        pure (a :% 1)

instance Num Number where
  (a :% b) + (c :% d) = reduce (a * d + c * b) (b * d)
  (a :% b) * (c :% d) = reduce (a * c) (b * d)
  negate (a :% b) = (-a) :% b
  abs (a :% b) = (abs a) :% b
  signum (a :% _) = (signum a) :% 1
  fromInteger i = i :% 1

instance Fractional Number where
  (a :% b) / (c :% d) = reduce (a * d) (b * c)
  recip (a :% b) = b :% a
  fromRational r = (Ratio.numerator r) :% (Ratio.denominator r)

instance Ord Number where
  (a :% 0) `compare` (c :% 0) = a `compare` c
  (a :% b) `compare` (c :% d) = (a * d) `compare` (b * c)

instance Enum Number where
  -- toEnum and fromEnum use Integer as a midpoint for converting to/from Int
  toEnum n = fromInteger $ toEnum n
  fromEnum (a :% b) = fromEnum $ a `quot` b
  -- Because fromEnum is partial, we have to define all the other Enum methods
  -- by hand
  succ n = n + 1
  pred n = n - 1
  enumFrom n = iterate succ n
  enumFromThen n n' = n : iterate (+ (n' - n)) n'
  enumFromTo n m = takeWhile (<= m) (enumFrom n)
  enumFromThenTo n n' m
    | n > n' = takeWhile (>= m) (enumFromThen n n')
    | otherwise = takeWhile (<= m) (enumFromThen n n')

instance Real Number where
  toRational (a :% b) = a Ratio.% b

instance RealFrac Number where
  properFraction (a :% b) = (fromInteger (a `quot` b), (a `rem` b) :% b )

instance Integral Number where
  quotRem n m
    | (_ :% 0) <- n / m = (n / m, 0)
    | (q, r) <- properFraction (n / m) = (q :% 1, r * m)
  toInteger n = fst $ properFraction n

-- Convert some other numeric type to a Number
toNumber :: Real a => a -> Number
toNumber = fromRational . toRational

-- The natural numbers, as Numbers
naturals :: [Number]
naturals = [0..]
