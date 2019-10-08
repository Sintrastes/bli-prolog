
module Data.Alias where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Empty

class (Traversable t, HasEmpty t) => Alias t where                                            
    getPrimaryId' :: (t String) -> String -> Maybe String
    addNewAlias'  :: (t String) -> (String, String) -> Maybe (t String)

instance Alias (Map String)  where
    getPrimaryId' store id = Map.lookup id store
    addNewAlias' store (id1, id2) =
      case (getPrimaryId' store id1) of
         Just primaryId1 -> Just $ Map.insert id2 primaryId1 store
         Nothing -> case (getPrimaryId' store id2) of
            Just primaryId2 -> Just $ Map.insert id1 primaryId2 store
            Nothing -> Nothing