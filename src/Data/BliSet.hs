
module Data.BliSet where

import Data.List

-- | Generic interface for a Set-like container.
class BliSet t where
  insert :: Eq a => a -> t a -> t a
  tryRemove :: Eq a => a -> t a -> Either (t a) (t a)
  lookup :: Eq a => a -> t a -> Maybe a

instance BliSet [] where
  insert x xs = nub $ xs ++ [x]
  tryRemove x xs 
      | length xs' == length xs = Left xs
      | otherwise = Right xs
    where xs' = filter (\y -> y /= x) xs
  lookup x xs = find (\y -> y == x) xs