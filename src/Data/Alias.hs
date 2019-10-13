
module Data.Alias where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Empty
import Data.BliSet (BliSet)
import qualified Data.BliSet as BliSet


class (Traversable t, HasEmpty t) => Alias t where
    getPID :: (t String) -> String -> Maybe String 
    insertNewAlias' :: (t String) -> (String, String) -> (t String)

instance Alias (Map String) where
    insertNewAlias' store (id1, id2) = Map.insert id1 id2 store
    getPID aliases id = Map.lookup id aliases