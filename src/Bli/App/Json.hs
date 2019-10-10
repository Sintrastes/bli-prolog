
module Bli.App.Json where

--
-- | Json parsing utilities for the bli-prolog executable.
-- 

import Data.Prolog.Ast
import Prolog.Interp.Data
import Data.Aeson.Encode.Pretty
import qualified Data.Map as Map
import Data.ByteString.Lazy.UTF8

-- | Formats a solution as an encoded Json string.
solutionToJson :: Solution -> String
solutionToJson (Solution list) = 
          (\xs -> toString $ encodePretty $ Map.fromList xs) 
          (map (\(x,y) -> (x, show y)) list)
