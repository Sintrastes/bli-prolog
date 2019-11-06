
module Control.Monad.Bli.Pure.Generic (
    Bli,
    module Control.Monad.Bli.Trans.Generic,
  ) where

import Control.Monad.Identity
import Control.Monad.Bli.Trans.Generic
import Data.Alias
import Data.BliSet
import Control.Monad.Trans.State.Strict

-- | A monad for wrapping pure computations done (and run) in bli prolog.
type Bli t1 t2 t3 t4 alias a = StateBliT t1 t2 t3 t4 alias Identity a
  
