
module Control.Monad.Bli.TH where

--
-- | Experimental: Quasiquoters to help us deal with overly-complicated constraints.
--

import Data.BliSet
import Data.Alias

bliMonadCtx x = [t| forall t1. forall t2. forall t3. forall alias. (BliSet t1, BliSet t2, BliSet t3, Alias alias) => $(x) |]

-- bliMnd a = [t| Bli t1 t2 t3 t2 alias $(a) |]