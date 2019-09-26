
--
-- Utility functions for analyzing
-- prolog terms.
--

module Prolog.Analysis where

import Data.Prolog.Ast
import Prolog.Interp (isPlain)
import qualified Data.Set as S
import Data.List((\\), nub)
import Control.Monad (join)
import Data.Schema

collectTermAtoms :: Term -> [(Atom, Int)]
collectTermAtoms x = nub $ go x
  where  go (Var _) = []
         go (Comp x ts) = (x, length ts):(join $ map go ts)

collectTermVars :: Term -> [Variable]
collectTermVars = nub . go
  where go (Var x) = [x]
        go (Comp _ ts) = join $ map go ts 

collectClauseAtoms :: Clause -> [(Atom, Int)]
collectClauseAtoms (t, ts) = nub $ collectTermAtoms t ++ (join $ map collectTermAtoms ts)

collectClauseVars :: Clause -> [Variable]
collectClauseVars (t, ts) = nub $ collectTermVars t ++ (join $ map collectTermVars ts)

collectProgramAtoms :: Program -> [(Atom, Int)]
collectProgramAtoms = nub . join . map collectClauseAtoms

collectProgramVars :: Program -> [Variable]
collectProgramVars = nub . join . map collectClauseVars

-- Checks to see what predicates are used in a bli prolog program.
bliUses :: BliProgram -> [String]
bliUses = undefined

-- Checks to see if a bli clause is valid with regard to a given schema.
isBliClauseValid :: BliCommand -> Schema -> Bool
isBliClauseValid = undefined
 
-- Checks to see if a bli program is valid with respect to the given schema.
isBliProgramValid :: BliProgram -> Schema -> Bool
isBliProgramValid = undefined
