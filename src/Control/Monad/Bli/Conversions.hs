
module Control.Monad.Bli.Conversions where

import Data.Alias
import Data.BliSet
import Control.Monad.Trans.State.Strict

-- For writing liftFromPure
import qualified Control.Monad.Bli.Pure.Generic as Pure
import qualified Control.Monad.Bli.Generic as State
import qualified Control.Monad.Bli.MVar.Generic as MVar
import qualified Control.Monad.Bli.IORef.Generic as IORef

-- Function definitions

-- Helper function to go from the pure to the impure version of the Bli monad.
liftFromPure :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Pure.Bli t1 t2 t3 t4 alias a -> State.Bli t1 t2 t3 t4 alias a
liftFromPure (State.StateBliT x) = do
  store <- State.getStore
  let store' = execState x store
  State.modifyStore (\_ -> store')
  return $ evalState x store

-- Helper function to go from the pure to the impure version of the Bli monad.
liftMVarFromPure :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Pure.Bli t1 t2 t3 t4 alias a -> MVar.Bli t1 t2 t3 t4 alias a
liftMVarFromPure (State.StateBliT x) = do
  store <- MVar.getStore
  let store' = execState x store
  MVar.modifyStore (\_ -> store')
  return $ evalState x store

liftIORefFromPure :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Pure.Bli t1 t2 t3 t4 alias a -> IORef.Bli t1 t2 t3 t4 alias a
liftIORefFromPure (State.StateBliT x) = do
  store <- IORef.getStore
  let store' = execState x store
  IORef.modifyStore (\_ -> store')
  return $ evalState x store

liftStateToMVar :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => State.Bli t1 t2 t3 t4 alias a -> MVar.Bli t1 t2 t3 t4 alias a
liftStateToMVar = undefined
