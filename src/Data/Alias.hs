
module Alias where

import Data.Map (Map)
import qualified Data.Map as Map

class Alias a where                                            
    getPrimaryId' :: a -> String -> Maybe String
    addNewAlias' :: a -> (String, String) -> Maybe a

instance Alias (Map String String)  where
    getPrimaryId' store id = lookup id store
    addNewAlias' store (id1, id2) =
      case (getPrimaryId' store id1) of
         Just primaryId1 -> Just $ Map.insert id2 primaryId1
         Nothing -> case (getPrimaryId' store) of
            Just primaryId2 -> Just $ Map.insert id1 primaryId2
            Nothing -> Nothing