
module Data.Alias where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Empty
import Data.BliSet (BliSet)
import qualified Data.BliSet as BliSet

-- t is a mapping for associating identifiers with their primary ids,
-- and p is a container for looking up primary ids.
class (Traversable t, HasEmpty t, BliSet p) => Alias t p where
    addNewPrimaryId' :: (p String) -> (t String) -> String -> Maybe (p String)
    getPrimaryId'    :: (p String) -> (t String) -> String -> Maybe String
    addNewAlias'     :: (p String) -> (t String) -> (String, String) -> Maybe (t String)

instance Alias (Map String) [] where
    addNewPrimaryId' pids _ pid = 
      case BliSet.tryInsert pid pids of
        Right result -> Just result
        Left  result -> Nothing
    getPrimaryId' pids store id = 
      case addNewPrimaryId' pids store id of
         -- id is already a primary id.
         Nothing -> Just id
         -- If id is not a primary id, look it up in the store.
         Just _ -> Map.lookup id store
    addNewAlias' pids store (id1, id2) =
      case (getPrimaryId' pids store id1) of
         Just primaryId1 -> Just $ Map.insert id2 primaryId1 store
         Nothing -> case (getPrimaryId' pids store id2) of
            Just primaryId2 -> Just $ Map.insert id1 primaryId2 store
            Nothing -> Nothing