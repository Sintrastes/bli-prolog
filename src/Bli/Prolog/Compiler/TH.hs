{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Bli.Prolog.Compiler.TH where

import Language.Haskell.TH
import Control.Monad.IO.Class
import Control.Monad.Bli
import Bli.Prolog.Modules

-- | The same as "getBliProgramFromFile", except 
--   preformed at compile-time, not at runtime.
getBliProgramFromFile' :: String -> Bli (Q Exp)
getBliProgramFromFile' filePath = do
  contents <- getBliProgramFromFile filePath
  return $ [e| contents |]

