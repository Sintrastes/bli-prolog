
module Control.Monad.Bli.IORef.Generic (
  Bli,
  module Control.Monad.Bli.Trans.Generic
) where

import Control.Monad.Bli.Trans.Generic
import Control.Monad.Bli.Common

type Bli t1 t2 t3 t4 alias a = IORefBliT t1 t2 t3 t4 alias IO a