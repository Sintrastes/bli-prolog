
module Data.Alias where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Empty
import qualified Data.BliSet as BliSet

-- t is a mapping for associating identifiers with their primary ids,
-- and p is a container for looking up primary ids.
class (Traversable t, HasEmpty t, BliSet p) => Alias t p where
    addNewPrimaryId' :: (p String) -> String -> Maybe (p String)
    getPrimaryId'    :: (p String) -> String -> Maybe String
    addNewAlias'     :: (t String) -> (String, String) -> Maybe (t String)

instance Alias (Map String) [] where
    addNewPrimaryId' pids pid = 
      case BliSet.tryInsert pid pids of
        Right result -> Just result
        Left  result -> Nothing
    getPrimaryId' store id = Map.lookup id store
    addNewAlias' store (id1, id2) =
      case (getPrimaryId' store id1) of
         Just primaryId1 -> Just $ Map.insert id2 primaryId1 store
         Nothing -> case (getPrimaryId' store id2) of
            Just primaryId2 -> Just $ Map.insert id1 primaryId2 store
            Nothing -> Nothing