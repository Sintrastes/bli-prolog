--
-- | Utility functions for analyzing
--   prolog terms.
--

module Prolog.Analysis where

import Data.Prolog.Ast
import Prolog.Interp (isPlain)
import qualified Data.Set as S
import Data.List((\\), nub)
import Control.Monad (join)
import Data.Schema

-- | Helper function. Checks to see which identifiers are used in a pure prolog term.
collectTermAtoms :: Term -> [(Atom, Int)]
collectTermAtoms x = nub $ go x
  where  go (Var _) = []
         go (Comp x ts) = (x, length ts):(join $ map go ts)

-- | Helper function. Checks to see which variables are used in a pure prolog term.
collectTermVars :: Term -> [Variable]
collectTermVars = nub . go
  where go (Var x) = [x]
        go (Comp _ ts) = join $ map go ts 

-- | Helper function. Checks to see which identifiers are used in a pure prolog clause.
collectClauseAtoms :: Clause -> [(Atom, Int)]
collectClauseAtoms (t, ts) = nub $ collectTermAtoms t ++ (join $ map collectTermAtoms ts)

-- | Helper function. Checks to see which variables are used in a pure prolog clause.
collectClauseVars :: Clause -> [Variable]
collectClauseVars (t, ts) = nub $ collectTermVars t ++ (join $ map collectTermVars ts)

-- | Helper function. Checks to see which identifiers are used in a pure prolog program.
collectProgramAtoms :: Program -> [(Atom, Int)]
collectProgramAtoms = nub . join . map collectClauseAtoms

-- | Helper function. Checks to see which variables are used in a pure prolog program.
collectProgramVars :: Program -> [Variable]
collectProgramVars = nub . join . map collectClauseVars

-- | Helper function. Checks to see which identifiers are used in a bli prolog command.
collectBliCommandAtoms :: BliCommand -> [(Atom,Int)]
collectBliCommandAtoms (QueryMode goal)       = nub . join . (map collectTermAtoms) $ goal
collectBliCommandAtoms (AssertMode goal)      = nub . join . (map collectTermAtoms) $ goal
collectBliCommandAtoms (AssertClause clause)  = collectClauseAtoms clause
collectBliCommandAtoms (LambdaQuery (_,goal)) = nub . join . (map collectTermAtoms) $ goal

-- | Helper function. Checks to see which variables are used in a bli prolog command.
collectBliCommandVars :: BliCommand -> [Variable]
collectBliCommandVars (QueryMode goal)       = nub . join . (map collectTermVars) $ goal
collectBliCommandVars (AssertMode goal)      = nub . join . (map collectTermVars) $ goal
collectBliCommandVars (AssertClause clause)  = collectClauseVars clause
collectBliCommandVars (LambdaQuery (_,goal)) = nub . join . (map collectTermVars) $ goal

-- | Helper function. Checks to see which identifiers are used in a bli prolog program.
collectBliProgramAtoms :: BliProgram -> [(Atom, Int)]
collectBliProgramAtoms = nub . join . map collectBliCommandAtoms

-- | Helper function. Checks to see which variables are used in a bli prolog program.
collectBliProgramVars :: BliProgram -> [Variable]
collectBliProgramVars = nub . join . map collectBliCommandVars

-- | Unit type, representing valid input.
data Ok = Ok

-- | Data type representing all of the possible errors
--   that can occur from validating the input to a bli prolog
--   command. 
data InvalidClause =
     -- | Error to return when a lambda query contains a bound variable which does not
     --   appear in the body.
     BoundVarNotInBody 
     -- | Error to return when a query (or an assertion) of any kind contains
     --   identifiers which do not exist in any of the imported schemas.
   | AtomsNotInSchema [Atom]

-- | Checks to see if a bli clause is valid with regard to a given schema.
isBliCommandValid :: BliCommand -> Schema -> Either InvalidClause Ok
isBliCommandValid x@(QueryMode goal) schema  
   | atoms `subset` schema = Right Ok
   | otherwise = Left $ AtomsNotInSchema $
                     map (\(x,y) -> x) 
                           (filter (\x -> not $ x `elem` schema) 
                            atoms)
 where atoms = collectBliCommandAtoms x
       subset xs ys = all (\x -> x `elem` ys) xs
isBliCommandValid x@(AssertMode goal) schema 
   | atoms `subset` schema = Right Ok
   | otherwise = Left $ AtomsNotInSchema $
                     map (\(x,y) -> x) 
                           (filter (\x -> not $ x `elem` schema) 
                            atoms)
  where atoms = collectBliCommandAtoms x
        subset xs ys = all (\x -> x `elem` ys) xs
isBliCommandValid x@(AssertClause clause) schema 
   | atoms `subset` schema = Right Ok
   | otherwise = Left $ AtomsNotInSchema $
                     map (\(x,y) -> x) 
                           (filter (\x -> not $ x `elem` schema) 
                            atoms)
  where atoms = collectBliCommandAtoms x
        subset xs ys = all (\x -> x `elem` ys) xs
isBliCommandValid x@(LambdaQuery (bindingVars,goal)) schema 
   | (bindingVars `subset` bodyVars) && (atoms `subset` schema) = Right Ok
   | not (bindingVars `subset` bodyVars) = Left $ BoundVarNotInBody
   | not (atoms `subset` schema) = Left $ AtomsNotInSchema $
                     map (\(x,y) -> x) 
                           (filter (\x -> not $ x `elem` schema) 
                            atoms) 
  where atoms = collectBliCommandAtoms x
        bodyVars = collectBliCommandVars x 
        subset xs ys = all (\x -> x `elem` ys) xs
 
-- | Checks to see if a bli program is valid with respect to the given schema.
isBliProgramValid :: BliProgram -> Schema -> Bool
isBliProgramValid = undefined
