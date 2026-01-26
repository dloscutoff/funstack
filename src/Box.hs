module Box (
  Box (..),
  extractAll
) where

-- A Box is an applicative structure that contains a Single item
-- or a Multiple list of Boxes
data Box a =
  Single a |
  Multiple [Box a]
  deriving Show

instance Functor Box where
  fmap f (Single x) = Single $ f x
  fmap f (Multiple bs) = Multiple $ map (fmap f) bs

instance Applicative Box where
  pure = Single
  (Single f) <*> b = fmap f b
  bf <*> (Single x) = fmap ($ x) bf
  (Multiple bfs) <*> (Multiple bs) = Multiple $ zipWith (<*>) bfs bs

-- Remove all the items from a Box and put them in a flat list
extractAll :: Box a -> [a]
extractAll (Single x) = [x]
extractAll (Multiple bs) = bs >>= extractAll
