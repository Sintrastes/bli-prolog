
module Control.Monad.Bli.Pure (
    module ReExport,
    Bli
  ) where

--
-- | A pure version of the Bli monad. 
--   Can be used to favilitate the use of bli prolog
--   as an EDSL witihn Haskell
--
--   Note: If used within the same application as Control.Monad.Bli,
--   must be imported qualified to avoid name conflicts.

import Data.Bli.Prolog.Ast
import Control.Applicative
import Control.Monad.State.Lazy
import Bli.App.Config (AppConfig)
import Control.Monad.Bli.Common
import Control.Monad.Bli.Pure.Generic as ReExport hiding (Bli)
import qualified Control.Monad.Bli.Pure.Generic as Generic
import qualified Control.Monad.Bli as Bli

-- | A monad for wrapping computations done (and run) in bli prolog.
type Bli a = Generic.Bli 
 -- | The container to use for the fact store
    FactContainer
 -- | The container to use for the relational store 
    RelationContainer
 -- | The container to use for the entity store
    EntityContainer
 -- | The container to use for the type store
    TypeContainer
 -- | The datastructure to use for storing aliases
    AliasDatastructure 
    a