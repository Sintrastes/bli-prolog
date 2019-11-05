
module Control.Monad.Bli.Pure.Generic (
    Bli,
    module Control.Monad.Bli.Trans.Generic,
    liftFromPure
  ) where

import Control.Monad.Identity
import Control.Monad.Bli.Trans.Generic
import Data.Alias
import Data.BliSet
import Control.Monad.Trans.State.Strict

-- For writing liftFromPure
import qualified Control.Monad.Bli.Generic as Bli

-- | A monad for wrapping pure computations done (and run) in bli prolog.
type Bli t1 t2 t3 t4 alias a = BliT t1 t2 t3 t4 alias Identity a

-- Helper function to go from the pure to the impure version of the Bli monad.
liftFromPure :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t4 alias a -> Bli.Bli t1 t2 t3 t4 alias a
liftFromPure x = do
  store <- Bli.getStore
  let store' = execState x store
  Bli.modifyStore (\_ -> store')
  return $ evalState x store
  