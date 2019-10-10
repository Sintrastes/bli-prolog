{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Bli.App.Config.Util where

--
-- | Some utility functions for dealing with extensible records, modeled 
-- with existential types.
--
-- The main use case I had in mind for this (and especially the #> and <# operators, pronounced
-- "overlay left", and "overlay right" respectively) was applications where configuration data
-- can come from multiple sources, such as a cli application which also reads from a configuration
-- file, but where the command line argument override the defaults set in the config file.
--
-- In this situation, one could take configDataAsRecord #> cmdArgsAsRecord and
-- pass it to a function such as toPerson to convert the extensible Record type
-- to a specific record type for the application configuration.
--
-- The typeclass IsRecord a defined at the end of this file can help to
-- facilitate this pattern.
--

import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad

-- A typeclass for datatypes which can be viewed as finite Maps
-- from record fields to generic types with an Eq instance.
class IsRecord a where
  toRecord :: a -> Record
  fromRecord :: Record -> Maybe a

-- | Container for types with an Eq instance.
data Typ = forall t. (Eq t, Typeable t) => Typ t

-- | Two values of Typ are equal if their types are equal, and
-- their wrapped values are equal.
instance Eq Typ where
  (Typ x) == (Typ y) =
     case (typeOf x) == (typeOf y) of
     -- This is safe since x and y are of the same type.
       True  -> x == fromJust (cast y)
       False -> False
    where fromJust (Just y) = y

-- | A generic, extensible record type, mapping String identifiers
-- to arbitrary types with an Eq instance.
type Record = Map String Typ

-- | Combine two compatible maps. Returning Nothing if the
-- maps are incompatible (i.e. if they have different values for
-- the same keys).
infixr 9 #
(#) :: (Ord k, Eq a) => Map k a -> Map k a -> Maybe (Map k a)
xs # ys = (foldr (>=>) (\x -> Just x) (map helper xs')) ys 
  where xs' = Map.toList xs
        helper (k, val) =
          case Map.lookup k ys of
            Just val' -> if val == val'
                         then \x -> Just x
                         else \x -> Nothing
            Nothing -> \x -> Just $ Map.insert k val x

-- | Combine any two maps, overriding with the value on the right if
-- incompatible.
infixr 9 #>
(#>) :: Ord k => Map k a -> Map k a -> Map k a
xs #> ys = foldr1 (.) (map helper xs') $ ys 
  where xs' = Map.toList xs
        helper (k, val) =
          case Map.lookup k ys of
            Just val' -> Map.insert k val'
            Nothing   -> Map.insert k val    

-- | Combine any two maps, overriding with the value on the left
-- if incompatible
infixr 9 <#
(<#) :: Ord k => Map k a -> Map k a -> Map k a
xs <# ys = ys #> xs
