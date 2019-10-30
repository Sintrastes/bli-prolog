
module Data.BliParser where

import Text.Parsec
import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Bli.Trans.Generic
import Control.Monad.Bli.Common
import Control.Monad.Trans.State

type GenericBliParser t1 t2 t3 t4 alias m a = ParsecT String () (StateT (BliStore t1 t2 t3 t4 alias) m) a

type BliParser a = GenericBliParser FactContainer RelationContainer EntityContainer TypeContainer AliasDatastructure IO a

bli = lift