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
import qualified Data.BliSet as BliSet

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
   | WrongArities [(Atom,Int)] 
-- ... Identifier X is being used as an Nary predicate, but is declared to
--     be a term of type Y in the schema.
   | NotAPredicate (String, Int, String)
-- ... Encountered type errors:                                                                         
--         In predicate X, argument n is not of type Y, but rather of type W.
   | TypeError (String, Int, String, String)
-- | type X has not been declared in the schema.
   | TypeNotDeclared String deriving(Eq, Show)

-- | Helper function for error accumulation.  
collectErrors :: [Either InvalidClause Ok] -> Either [InvalidClause] Ok
collectErrors xs = collectErrors' xs []
  where collectErrors' ((Left err):xs) acc = collectErrors' xs (err:acc)
        collectErrors' ((Right Ok):xs) acc = collectErrors' xs acc
        collectErrors' [] []  = Right Ok
        collectErrors' [] acc = Left acc

-- | Helper function for joining errors.
joinErrors :: Either [InvalidClause] Ok -> Either [InvalidClause] Ok -> Either [InvalidClause] Ok
joinErrors (Left xs) (Left ys)   = Left (xs ++ ys)
joinErrors (Left xs) (Right Ok)  = Left xs
joinErrors (Right Ok) (Left xs)  = Left xs
joinErrors (Right Ok) (Right Ok) = Right Ok

typecheckTerm :: Term -> Bli (Either [InvalidClause] Ok)
typecheckTerm (Var x) = return $ Right Ok
typecheckTerm (Comp p xs) = do
  types     <- getTypes
  relations <- getRelations
  entities  <- getEntities
  aliases   <- getAliases
  -- First, check if head of the term is a predicate.
  case BliSet.lookup (\(a,b)-> a==p) relations of
    Nothing -> case BliSet.lookup (\(a,b) -> a == p) entities of
                 Just (_,t)  -> return $ Left $ [NotAPredicate (p,length xs,t)]
                 Nothing -> return $ Left $ [AtomsNotInSchema [p]]
    Just (_, expectedTypes) -> do
      -- Helper function
      let termHead (Var x) = x
          termHead (Comp x _) = x
      -- Typecheck each of the individual arguments 
      -- of the predicate.
      let intermediateResults = map 
            (\(expectedType, x, n) -> 
             -- Find the type of x in the entity store
             case BliSet.lookup (\(a,b) -> x==a) entities of
               Nothing -> Left $ TypeNotDeclared x
               Just (_,typeOfX)  -> 
                 if typeOfX == expectedType
                 then Right Ok
                 else Left $ TypeError (p, n, expectedType, typeOfX))
               (zip3 expectedTypes (map termHead xs) [1..length xs])
      -- Return all of the errors that were encountered, or none
      -- if no errors were encountered.
      return $ collectErrors intermediateResults

typecheckGoal :: Goal -> Bli (Either [InvalidClause] Ok)
typecheckGoal terms = do
  results <- mapM typecheckTerm terms
  return $ foldr joinErrors (Right Ok) results

typecheckBliCommand :: BliCommandTyped -> Bli (Either [InvalidClause] Ok)
typecheckBliCommand (T_QueryMode goal) = typecheckGoal goal
typecheckBliCommand cmd@(T_LambdaQuery (bindingVars, terms)) = do
  result <- typecheckGoal terms
  let bodyVars = collectTypedBliCommandVars cmd 
  let lambdaError = 
        if (bindingVars `subset` bodyVars)
        then Right Ok
        else Left $ [BoundVarNotInBody]
  return $ joinErrors lambdaError result

typecheckBliCommand (T_AssertMode terms) = typecheckGoal terms
typecheckBliCommand (T_AssertClause (t,ts)) = do
  results1 <- typecheckGoal ts
  results2 <- typecheckTerm t
  return $ joinErrors results1 results2
-- Asserting schemas is not covered by typechecking.
typecheckBliCommand (T_AssertSchema _) = return $ Right Ok

-- Note: We could probably refactor this by making seperate "typecheck goal" and
-- "typecheck clause" functions.
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
