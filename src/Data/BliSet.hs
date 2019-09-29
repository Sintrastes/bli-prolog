
module Data.BliSet where

import Data.List
import Data.Set (Set)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import Data.Sequence (Seq(..), (|>), (!?), dropWhileL, findIndexL)
import qualified Data.Sequence as Seq

-- | Generic interface for a Set-like container.
class BliSet t where
  empty :: t a
  tryInsert :: Eq a => a -> t a -> Either (t a) (t a)
  tryRemove :: Eq a => a -> t a -> Either (t a) (t a)
  lookup :: Eq a => (a -> Bool) -> t a -> Maybe a

instance BliSet [] where
  empty = []
  tryInsert x xs
      | x `elem` xs = Left xs
      | otherwise = Right $ xs ++ [x]
  tryRemove x xs 
      | length xs' == length xs = Left xs
      | otherwise = Right xs
    where xs' = filter (\y -> y /= x) xs
  lookup pred xs = find pred xs

-- Another example instance of BliSet (although, I don't think this is going to be
-- any more efficent than the list implementation.
instance BliSet Seq where
  empty = Empty
  tryInsert x xs = case findIndexL (\y -> x == y) xs of 
      (Just _) -> Left xs
      Nothing  -> Right $ xs |> x
  lookup pred xs = (findIndexL pred xs) >>= (xs !?)
  tryRemove x xs
     | length xs' == length xs = Left xs
     | otherwise = Right xs'
   where xs' = dropWhileL (\y -> x == y) xs

instance BliSet Set where
  empty = Set.empty

-- Note: A hash set needs a hashable instance.
--       This is probably not what we want anyway.
{-
instance BliSet HashSet where
  empty = HashSet.empty
  tryInsert x xs = case HashSet.member x xs of
      True  -> Left  $ xs
      False -> Right $ HashSet.insert x xs
  lookup pred xs = undefined
-}