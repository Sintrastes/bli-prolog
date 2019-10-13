
module Data.Alias where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Empty
import Data.BliSet (BliSet)
import qualified Data.BliSet as BliSet


class (Traversable t, HasEmpty t) => Alias t where
    insertNewAlias' :: (t String) -> (String, String) -> (t String)

instance Alias (Map String) where
    insertNewAlias' store (id1, id2) = Map.insert id1 id2 store
    --  case (getPrimaryId' store id1) of
    --     Just primaryId1 -> Just $ Map.insert id2 primaryId1 store
    --     Nothing -> case (getPrimaryId' store id2) of
    --        Just primaryId2 -> Just $ Map.insert id1 primaryId2 store
    --        Nothing -> Nothing