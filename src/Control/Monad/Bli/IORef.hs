
module Control.Monad.Bli.IORef (
  Bli,
  module Control.Monad.Bli.MVar.Generic
) where

import Control.Monad.Bli.MVar.Generic hiding (Bli)
import qualified Control.Monad.Bli.IORef.Generic as Generic
import Control.Monad.Bli.Common

type Bli a = Generic.Bli FactContainer RelationContainer EntityContainer TypeContainer AliasDatastructure a