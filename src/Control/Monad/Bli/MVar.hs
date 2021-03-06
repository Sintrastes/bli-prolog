
--
-- | A version of the Bli monad that uses an MVar
--   to hold the BliStore.
--

module Control.Monad.Bli.MVar (
  Bli,
  module Control.Monad.Bli.MVar.Generic
) where

import Control.Monad.Bli.MVar.Generic hiding (Bli)
import qualified Control.Monad.Bli.MVar.Generic as Generic
import Control.Monad.Bli.Common

type Bli a = Generic.Bli FactContainer RelationContainer EntityContainer TypeContainer AliasDatastructure a