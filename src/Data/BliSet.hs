
module Data.BliSet where

import Prelude   hiding (filter, union, lookup)
import Data.List hiding (filter, lookup, union, (\\))
import qualified Data.List as List
import Data.Set (Set, (\\), singleton)
import qualified Data.Set as Set
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Sequence (Seq(..), (|>), (><), (!?), dropWhileL, findIndexL)
import qualified Data.Sequence as Seq
import Control.Empty
import Data.Witherable

-- | Generic interface for a Set-like container.
class (Traversable t, Filterable t, HasEmpty t) => BliSet t where
  tryInsert :: Ord a => a -> t a -> Either (t a) (t a)
  tryRemove :: Ord a => a -> t a -> Either (t a) (t a)
  lookup :: Ord a => (a -> Bool) -> t a -> Maybe a
  union  :: Ord a => t a -> t a -> t a

isIn :: (BliSet t, Eq a, Ord a) => a -> t a -> Bool
isIn x xs = 
  case lookup (==x) xs of
    (Just _) -> True
    Nothing  -> False
    
instance BliSet [] where
  tryInsert x xs
      | x `elem` xs = Left xs
      | otherwise = Right $ xs ++ [x]
  tryRemove x xs 
      | length xs' == length xs = Left xs
      | otherwise = Right xs
    where xs' = filter (\y -> y /= x) xs
  lookup pred xs = find pred xs
  union = List.union

-- Another example instance of BliSet (although, I don't think this is going to be
-- any more efficent than the list implementation.
instance BliSet Seq where
  tryInsert x xs = case findIndexL (\y -> x == y) xs of 
      (Just _) -> Left xs
      Nothing  -> Right $ xs |> x
  lookup pred xs = (findIndexL pred xs) >>= (xs !?)
  tryRemove x xs
     | length xs' == length xs = Left xs
     | otherwise = Right xs'
   where xs' = dropWhileL (\y -> x == y) xs
  -- Note: This implementation isn't really ideal. Need to use "nub", but for sequences.
  union xs ys = xs >< ys

-- Note: This doesn't work now because Set has no Traversable or Functor
-- instances.
--
-- We could probably give it some, but then I'm not sure how effeicent it would
-- be, and I'm not sure if this is actually needed for our use case.
--
{-
instance BliSet Set where
  lookup = find
  tryRemove x xs 
    | Set.member x xs = Right $ xs \\ (singleton x) 
    | otherwise   = Left $ xs    
  tryInsert x xs 
    | Set.member x xs = Right $ Set.insert x xs  
    | otherwise   = Left $ xs
  union = Set.union
-}

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