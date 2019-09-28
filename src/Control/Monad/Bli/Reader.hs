
module Control.Monad.Bli.Reader where

import Data.Prolog.Ast
import Data.Schema
import Control.Monad.Reader
import Bli.App.Config
import Control.Applicative

type BliReader = Reader (Options, Program, Schema)

askOpts :: BliReader Options
askOpts = fmap (\(x,y,z) -> x) ask

askProgram :: BliReader Program
askProgram = fmap (\(x,y,z) -> y) ask

askSchema :: BliReader Schema
askSchema = fmap (\(x,y,z) -> z) ask