
module Bli.Prolog.Interp where

import Prelude hiding (mapM, join)
import Data.List (nub)
import Control.Monad(liftM, join)
import Control.Monad.Bli
import Data.Foldable
import Data.BliSet (isIn)
import Data.Alias
import Data.Maybe
import Data.Traversable (mapM)
import Control.Monad.Trans.Class
import Data.Bli.Prolog.Ast
import Bli.Prolog.Unification
import Bli.Prolog.Unification.Terms
import Bli.Prolog.Interp.Data
import Bli.Prolog.Unification

----------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------

-- | Helper function to get just the parts of the goal that
--   consist of *type predicates* -- i.e. predicates of the form
--   id(X), where id is the identifier for a type in the current schema.
getTypePredicates :: Goal -> Bli Goal
getTypePredicates goal = do
  types <- getTypes
  let termHead (Var x) = x
      termHead (Comp (Identifier x) _) = x
  return $ filter (\x -> termHead x `isIn` types) goal

-- | Helper function to return a list of all of the stored
--   relations (along with their type identifiers) which
--   are used in a goal.
--
--   Used in solve so that we can apply the corrent logic
--   for relations that are stored in bedelibry.
getStoredRelations :: Goal -> Bli (String, [String])
getStoredRelations goal = undefined

-- | Helper function that replaces all occurences of 
--   ids in a term with their primary id, if it exists. 
expandAliasesTerm :: Term -> Bli Term
expandAliasesTerm (Comp (Identifier x) ts) = do
   pidX <- fromMaybe x <$> lookupPrimaryID x
   args <- mapM expandAliasesTerm ts
   return (Comp (Identifier pidX) args)
expandAliasesTerm (Var x) = return $ Var x
expandAliasesTerm x = return x

-- | Helper function that replaces all occurences of 
--   ids in a goal with their primary id, if it exists. 
expandAliases :: Goal -> Bli Goal
expandAliases terms = mapM expandAliasesTerm terms

-- Note: Here is where we can put our custom logic for
-- e.x. dealing with the bedelibry backend.
solve :: Goal -> Bli [SearchTree]
solve goal' = do
  prog <- toList <$> getFacts
  aliases <- getAliases
  goal <- expandAliases goal'
  typePredicates <- getTypePredicates goal
  case () of
   _ | typePredicates /= [] -> do
        -- Collection of types used in the type predicates
        let types = fmap (\(Comp (Identifier x) ts) -> x) typePredicates
        -- Lists of all entities of the types in the type predicates
        entityLists <- mapM getEntitiesOfType types
        -- Get all of the type predicate facts that we need.
        let newFacts = join $ fmap (\(entities, typ) -> fmap (\x -> (Comp (Identifier typ) [Comp (Identifier x) []],[])) entities) 
                          $ zip entityLists types
        -- Bring them into scope.
        newScopedFacts newFacts "_temp_"
        -- Get the new list of facts after adding our scoped facts.
        prog <- toList <$> getFacts
        let solution = solve' prog goal
        -- Bring the facts we needed out of scope.
        clearScope "_temp_"
        return solution

     | otherwise -> return $ solve' prog goal

-- Uses the List monad for backtracking
solve' :: Program -> Goal -> [SearchTree]
solve' _ [r] | isReportGoal r = [Sol $ getSolution r]
solve' prog g@(t1 : ts) = [Node g trees]
    where trees = do c <- prog
                     let (tc, tsc) = freshen (join $ map variables g) c
                     case unify tc t1 of
                       Just u -> do
                         let g' = map (subs u) $ tsc ++ ts
                         solve' prog g'
                       Nothing -> []
--solve _ _ = []

makeReportGoal goal = [Comp (Identifier "_report") reportVars]
    where reportVars = map (\ v -> Comp (Identifier "=") [Comp (Identifier v) [], Var v]) vars
          vars = join $ map variables goal

isReportGoal (Comp (Identifier "_report") _) = True
isReportGoal _                  = False

getSolution (Comp (Identifier "_report") args) = Solution sol
    where sol = filter nontriv $ map (\ (Comp (Identifier "=") [Comp (Identifier v) [], t]) -> (v, t)) args
          nontriv (x, (Var y)) | x == y = False
          nontriv _ = True
getSolution _ = error "getSolution should never be called like this"

-- Use the trick of inserting an extra reporting goal
makeReportTree :: Goal -> Bli SearchTree
makeReportTree goal = do 
  branches <- solve (goal ++ makeReportGoal goal)
  return $ Node goal branches 

