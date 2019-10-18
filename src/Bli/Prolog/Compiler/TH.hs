{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Bli.Prolog.Compiler.TH where

import Language.Haskell.TH
import Control.Monad.IO.Class
import Bli.Prolog.Modules

-- | The same as "getBliProgramFromFile", except 
--   preformed at compile-time, not at runtime.
getBliProgramFromFile' :: String -> Q Exp
getBliProgramFromFile' filePath= do
  contents <- liftIO $ getBliProgramFromFile filePath
  [e| contents |]

