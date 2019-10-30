{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Bli.Prolog.Compiler.TH where

import Language.Haskell.TH
import Control.Monad.IO.Class
import Control.Monad.Bli
import qualified Control.Monad.Bli.Pure as Pure
import Bli.Prolog.Modules
import Control.Applicative
import Bli.App.Config.Data

-- | The same as "getBliProgramFromFile", except 
--   preformed at compile-time, not at runtime.
getBliProgramFromFile' :: AppConfig -> String -> Q Exp
getBliProgramFromFile' opts filePath = do
  contents <- liftIO $ initBli opts $ getBliProgramFromFile filePath
  case contents of
    Nothing -> [e| Nothing |]
    Just file -> [e| file |]

