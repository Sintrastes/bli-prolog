
module Control.Monad.Bli.Generic (
  Bli,
  module Control.Monad.Bli.Trans.Generic
) where

import Control.Monad.Bli.Trans.Generic

type Bli t1 t2 t3 t4 alias a = StateBliT t1 t2 t3 t4 alias IO a
