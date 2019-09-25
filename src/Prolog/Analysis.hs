
--
-- Utility functions for analyzing
-- prolog terms.
--

module Prolog.Analysis where

import Data.Prolog.Ast
import Prolog.Interp (isPlain)
import qualified Data.Set as S
import Data.List((\\))
import Data.Schema

-- (?) Not sure what this does
makeSig :: String -> [a] -> String
makeSig f args = atom++"/"++(show $length args)
  where atom = if isPlain f then f else "'" ++ f ++ "'"

-- (?) Not sure what this does.
interface :: Program -> [String]
interface prog = collect prog S.empty
  where collect [] acc = S.toList acc
        collect ((head,_) : rest) acc =
          case head of
          Comp f args -> collect rest $ S.insert (makeSig f args) acc
          _           -> collect rest acc


-- Checks to see what predicates are used in a program (?)
uses :: Program -> [String]
uses prog = collect prog S.empty
  where collect [] acc = S.toList acc
        collect ((_, body) : rest) acc =
          collect rest $ foldl getUse acc body
        getUse acc (Comp f args) = S.insert (makeSig f args) acc
        getUse acc _             = acc


-- Checks to see what predicates are used in a bli prolog program.
bliUses :: BliProgram -> [String]
bliUses = undefined

-- Checks to see if a bli clause is valid with regard to a given schema.
isBliClauseValid :: BliCommand -> Schema -> Bool
isBliClauseValid = undefined
 
-- Checks to see if a bli program is valid with respect to the given schema.
isBliProgramValid :: BliProgram -> Schema -> Bool
isBliProgramValid = undefined

-- predicates used, which are not declared in the program. That is, uses\interface.
external :: Program -> [String]
external prog = uses prog \\ interface prog
