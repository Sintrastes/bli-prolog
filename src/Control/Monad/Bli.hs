
module Control.Monad.Bli(
    Bli,
    module ReExport,
  ) where

--
-- | A monad for IO computations preformed in the context of a 
--   running bli prolog session.
--
--   Note: If used within the same application as Control.Monad.Bli,
--   must be imported qualified to avoid name conflicts.
--

import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Schema
import Bli.App.Config (AppConfig)
import Control.Monad.Bli.Generic as ReExport hiding (Bli(..))
import qualified Control.Monad.Bli.Generic as Generic
import Control.Monad.Bli.Common

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