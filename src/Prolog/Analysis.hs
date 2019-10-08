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
import Control.Monad.Bli.Pure

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
collectBliCommandAtoms (AssertTypePred _)     = []

-- | Helper function. Checks to see which identifiers are used in a bli prolog command.
collectTypedBliCommandAtoms :: BliCommandTyped -> [(Atom,Int)]
collectTypedBliCommandAtoms (T_QueryMode goal)       = nub . join . (map collectTermAtoms) $ goal
collectTypedBliCommandAtoms (T_AssertMode goal)      = nub . join . (map collectTermAtoms) $ goal
collectTypedBliCommandAtoms (T_AssertClause clause)  = collectClauseAtoms clause
collectTypedBliCommandAtoms (T_LambdaQuery (_,goal)) = nub . join . (map collectTermAtoms) $ goal
collectTypedBliCommandAtoms (T_AssertSchema _) = []

-- | Helper function. Checks to see which variables are used in a bli prolog command.
collectBliCommandVars :: BliCommand -> [Variable]
collectBliCommandVars (QueryMode goal)       = nub . join . (map collectTermVars) $ goal
collectBliCommandVars (AssertMode goal)      = nub . join . (map collectTermVars) $ goal
collectBliCommandVars (AssertClause clause)  = collectClauseVars clause
collectBliCommandVars (LambdaQuery (_,goal)) = nub . join . (map collectTermVars) $ goal
collectBliCommandVars (AssertTypePred _)     = []

-- | Helper function. Checks to see which variables are used in a bli prolog command.
collectTypedBliCommandVars :: BliCommandTyped -> [Variable]
collectTypedBliCommandVars (T_QueryMode goal)       = nub . join . (map collectTermVars) $ goal
collectTypedBliCommandVars (T_AssertMode goal)      = nub . join . (map collectTermVars) $ goal
collectTypedBliCommandVars (T_AssertClause clause)  = collectClauseVars clause
collectTypedBliCommandVars (T_LambdaQuery (_,goal)) = nub . join . (map collectTermVars) $ goal
collectTypedBliCommandVars (T_AssertSchema _) = []

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
   -- | A list of atoms with invalid arities.
   | WrongArities [(Atom,Int)] deriving(Eq, Show)

-- | Utility function to find the arity usages of atoms that 
--   are not declared as valid in a schema, given a list of atoms
--   together with a list of their invalid arities.
nonMatchingArities :: [(Atom,[Int])] -> [(Atom, Int)] -> [(Atom,Int)]
nonMatchingArities atomsWithValidArities atoms = 
  filter (\(id, arity) -> toBool $
             (lookup id atomsWithValidArities)
           >>= (\x -> fromBool $ not $ arity `elem` x)) atoms
  where toBool (Just _) = True
        toBool Nothing  = False
        fromBool True   = Just ()
        fromBool False  = Nothing 

-- Note: There is a lot of refactoring I can do here regardless of the refactoring to the
-- typed version.

-- Helper functions used below.
subset xs ys = all (\x -> x `elem` ys) xs
getAritiesTerm val schema = (val, map snd $ filter (\(x,y) -> x == val) schema)

-- | Checks to see if a bli clause is valid in the given applicationContext.
isBliCommandValid :: BliCommandTyped -> Bli (Either InvalidClause Ok)
isBliCommandValid cmd@(T_QueryMode goal) = do
  predicates <- getRelations
  let schema = fmap (\(x, ts) -> (x, length ts)) predicates
  let atomsWithArities = map (\x -> getAritiesTerm (fst x) schema) atoms
  return $ case () of
    _ | atoms `subset` schema -> Right Ok
      | nonMatchingArities atomsWithArities atoms /= [] ->
           Left $ WrongArities $ nonMatchingArities atomsWithArities atoms
      | otherwise -> Left $ AtomsNotInSchema $
                         map (\(x,y) -> x) 
                               (filter (\x -> not $ x `elem` schema) 
                                atoms)
 where atoms = collectTypedBliCommandAtoms cmd
isBliCommandValid cmd@(T_AssertMode goal) = do
  predicates <- getRelations
  let schema = fmap (\(x, ts) -> (x, length ts)) predicates
  let atomsWithArities = map (\x -> getAritiesTerm (fst x) schema) atoms
  return $ case () of
   _ | atoms `subset` schema -> Right Ok
     | nonMatchingArities atomsWithArities atoms /= [] ->
             Left $ WrongArities $ nonMatchingArities atomsWithArities atoms
     | otherwise -> Left $ AtomsNotInSchema $
                       map (\(x,y) -> x) 
                             (filter (\x -> not $ x `elem` schema) 
                              atoms)
  where atoms = collectTypedBliCommandAtoms cmd
isBliCommandValid cmd@(T_AssertClause clause) = do
  predicates <- getRelations
  let schema = fmap (\(x, ts) -> (x, length ts)) predicates
  let atomsWithArities = map (\x -> getAritiesTerm (fst x) schema) atoms
  return $ case () of
   _ | atoms `subset` schema -> Right Ok
     | nonMatchingArities atomsWithArities atoms /= [] ->
          Left $ WrongArities $ nonMatchingArities atomsWithArities atoms
     | otherwise -> Left $ AtomsNotInSchema $
                       map (\(x,y) -> x) 
                             (filter (\x -> not $ x `elem` schema) 
                              atoms)
  where atoms = collectTypedBliCommandAtoms cmd
isBliCommandValid cmd@(T_LambdaQuery (bindingVars,goal)) = do 
  predicates <- getRelations
  let schema = fmap (\(x, ts) -> (x, length ts)) predicates
  let atomsWithArities = map (\x -> getAritiesTerm (fst x) schema) atoms
  return $ case () of
    _ | (bindingVars `subset` bodyVars) && (atoms `subset` schema) -> Right Ok
      | not (bindingVars `subset` bodyVars) -> Left $ BoundVarNotInBody
      | nonMatchingArities atomsWithArities atoms /= [] ->
           Left $ WrongArities $ nonMatchingArities atomsWithArities atoms
      | otherwise -> Left $ AtomsNotInSchema $
                        map (\(x,y) -> x) 
                               (filter (\x -> not $ x `elem` schema) 
                                atoms) 
  where atoms = collectTypedBliCommandAtoms cmd
        bodyVars = collectTypedBliCommandVars cmd 
-- | Checks to see if a bli program is valid in the given application context.
isBliProgramValid :: BliProgram -> Bli Bool
isBliProgramValid = undefined
