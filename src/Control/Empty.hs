
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Control.Empty where

import qualified Control.Applicative             as AP
import qualified Data.Map                        as Map
import qualified Data.Sequence                   as Seq
import qualified Data.Set                        as Set

class HasEmpty f where
    empty :: forall a. f a
    default empty :: AP.Alternative f => f a
    empty = AP.empty

instance HasEmpty Maybe
instance HasEmpty []
instance HasEmpty IO
instance HasEmpty (Map.Map k)   where empty = Map.empty
instance HasEmpty Seq.Seq       where empty = Seq.empty
instance HasEmpty Set.Set       where empty = Set.empty