--
-- | Implementation of typechecking for
--   bli prolog.
--

module Bli.Prolog.Typechecking (
    module Bli.Prolog.Typechecking.Data
  , module Bli.Prolog.Typechecking
 ) where

import Bli.Prolog.Typechecking.Data
import Bli.Util
import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Types
import Bli.Prolog.Interp (expandAliases, expandAliasesTerm)
import Bli.Prolog.Interp.Data (isPlain)
import qualified Data.Set as S
import Data.List((\\), nub, intercalate)
import Data.Char
import Control.Monad (join)
import Data.Bli.Prolog.Schema
import Control.Monad.IO.Class
import Control.Monad.Bli
import Data.BliSet (tryInsert)
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

collectGoalVars :: Terms -> [Variable]
collectGoalVars goal = nub $ join $ map collectTermVars goal

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
collectTypedBliCommandAtoms :: BliCommand -> [(Atom,Int)]
collectTypedBliCommandAtoms (Assert goal)      = nub . join . (map collectTermAtoms) $ goal
collectTypedBliCommandAtoms (AssertClause clause)  = collectClauseAtoms clause
collectTypedBliCommandAtoms (Query (_,goal)) = nub . join . (map collectTermAtoms) $ goal
collectTypedBliCommandAtoms (AssertSchema _) = []

-- | Helper function. Checks to see which variables are used in a bli prolog command.
collectTypedBliCommandVars :: BliCommand -> [Variable]
collectTypedBliCommandVars (Assert goal)      = nub . join . (map collectTermVars) $ goal
collectTypedBliCommandVars (AssertClause clause)  = collectClauseVars clause
collectTypedBliCommandVars (Query (_,goal)) = nub . join . (map collectTermVars) $ goal
collectTypedBliCommandVars (AssertSchema _) = []

-- Subtyping relation.
infixr 9 <:
(<:) :: BliPrologType -> BliPrologType -> Bli Bool
(<:) _ TypTypesT = return $ True
(<:) (DeclaredTypeT x) EntityT = return $ True
(<:) IntLitT FloatLitT = return $ True
-- (<:) (DeclaredTypeT x) (DeclaredTypeT y) = do
  -- check for either datatype structural subtyping,
  -- and/or entity structural subtyping if it is enabled.
-- Anything is a subtype of itself.
(<:) x y | x == y = return $ True
(<:) _ _ = return $ False

-- | Computes the least upper bound of two BliPrologTypes.
joinTypes :: BliPrologType -> BliPrologType -> BliPrologType
joinTypes = undefined

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

-- Note: It is important to make a distinction here
-- between atoms and strings, because atoms
-- have already been parsed, and hence, all of the
-- validation logic is done, and this can be a total function.

-- | Helper function to return the type of an atom which has already been parsed.
typeOfAtom :: Atom -> Bli (Maybe BliPrologType)
typeOfAtom (AtomVar x) = return $ Just $ AnyT
typeOfAtom (StringLiteral string) = return $ Just $ StringLitT
typeOfAtom (IntLiteral x) = return $ Just $ IntLitT
typeOfAtom (FloatLiteral x) = return $ Just $ FloatLitT
typeOfAtom (DataLit name args) = do
  -- lookup user-defined datatypes in scope,
  -- and see if our DataLit matches any of the types
  -- in our schema.
  maybeConstrType <- lookupTypeOfDataConstr name
  case maybeConstrType of
    Nothing -> return $ Nothing
    Just constrType -> return $ Just $ DeclaredTypeT constrType
typeOfAtom (AppTerm p xs) = do 
  -- Check to see p is a predicate, and all of its arguments are 
  -- of the correct type.
  relations <- getRelations
  entities  <- getEntities
  types     <- getTypes
  let relLookup = BliSet.lookup (\(a,b) -> p == a) relations
  case relLookup of
    Just (_, expectedTypes) -> do
      -- This is wrong
      intermediateResults <- checkThatArgumentsMatchExpectedTypes p (map (\x -> Comp x []) xs) (map readType expectedTypes)
      case collectErrors intermediateResults of
        Left _  -> return $ Nothing
        Right _ -> return $ Just $ PredicateT $ map readType expectedTypes
    Nothing -> return $ Nothing
typeOfAtom (ListLiteral xs) = do
  -- Take the join of the types of all of the xs.
  undefined
typeOfAtom (Rule _ _) = return $ Just $ RuleT
typeOfAtom (Goal goal) = do
  -- Lookup all free variables in the goal,
  let freeVars = collectGoalVars goal
  -- Lookup their types to determine
  -- what type of predicate this is.
  -- Note: These vars might be in multiple positions,
  -- and hence, we have to make sure that the
  -- expected types in these positions are all compatible
  -- with eachother.
  undefined
typeOfAtom (TimeperiodLiteral _) = return $ Just $ DateTimeLitT
typeOfAtom (Identifier string) 
  -- A variable has no type.
  -- Note: I don't think this check is needed now that we can represent variables in atoms
  -- more directly
  | isUpper (head string)            = return $ Nothing
  | otherwise = do
      -- Check to see if the literal has a user-declared
      -- type
      types     <- getTypes
      entities  <- getEntities
      relations <- getRelations
      let entityLookup = BliSet.lookup (\(a,b) -> string==a) entities
      return $ case entityLookup of
        Just (_,typeOfX) -> Just $ DeclaredTypeT typeOfX 
        Nothing ->  do
          -- Check to see if the literal is in fact a type.
          let typeLookup = BliSet.lookup (==string) types
          case typeLookup of
            Just _ -> Just $ TypTypesT
            Nothing -> do 
              -- Check to see the literal is a predicate
              let relnLookup = BliSet.lookup (\(a,b) -> string==a) relations
              case relnLookup of
                -- Again, this is a hack, and not even correct for all cases.
                Just (_, argTypes) -> Just $ PredicateT $ map DeclaredTypeT argTypes
                Nothing -> Nothing

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

-- checkThatArgumentsMatchExpectedTypes ::
checkThatArgumentsMatchExpectedTypes p xs expectedTypes = 
  mapM (\(expectedType, x, n) -> do
             typeOfX <- typeOfAtom x
             if typeOfX == Just expectedType
             then return $ Right Ok
             -- The formatting here isn't the most ideal.
             else case typeOfX of
                    Nothing -> case x of
                      -- Variables do not need type-checking in this case.
                        Identifier str | isUpper (head str) -> return $ Right Ok
                        _ -> return $ Left $ EntityNotDeclared (show x) (show expectedType)
                    Just typeOfX -> do
                      subTypeOfExpected <- typeOfX <: expectedType
                      if subTypeOfExpected
                      then return $ Right Ok
                      else return $ Left $ TypeError (p, n, show expectedType, show typeOfX))
 (zip3 expectedTypes (map termHead xs) [1..length xs])
   
readType x = 
  case x of
    "string"  -> StringLitT
    "period"  -> DateTimeLitT
    "int"     -> IntLitT
    "float"   -> FloatLitT
    "entity"  -> EntityT
    "rule"    -> RuleT
    otherwise -> DeclaredTypeT x

typecheckTerm :: Term -> Bli (Either [InvalidClause] Ok)
typecheckTerm (Var x) = return $ Right Ok
typecheckTerm (Comp (Identifier p) xs) = do
  types     <- getTypes
  relations <- getRelations
  entities  <- getEntities
  aliases   <- getAliases
  -- Note: I should refactor this so that it collects all errors for a given term.
  -- First, check if head of the term is a predicate.
  case BliSet.lookup (\(a,b)-> a==p) relations of
    Nothing -> case BliSet.lookup (\(a,b) -> a == p) entities of
                 Just (_,t)  -> return $ Left $ [NotAPredicate (p,length xs,t)]
                 Nothing -> return $ Left $ [AtomsNotInSchema [p]]
    Just (_, expectedTypes') -> do
      -- Quick fix. The better approach to this is probably re-writing our RelDecls
      let expectedTypes = map readType expectedTypes'
      -- Typecheck each of the individual arguments 
      -- of the predicate.
      intermediateResults <- checkThatArgumentsMatchExpectedTypes p xs expectedTypes
      return $ collectErrors intermediateResults
typecheckTerm (Comp x xs) = do
  typeOfX <- typeOfAtom x
  return $ Left $ [NotAPredicate (show x, length xs, show typeOfX)]

typecheckGoal :: Goal -> Bli (Either [InvalidClause] Ok)
typecheckGoal terms = do
  results <- mapM typecheckTerm terms
  return $ foldr joinErrors (Right Ok) results

typecheckBliCommand :: BliCommand -> Bli (Either [InvalidClause] Ok)
typecheckBliCommand cmd@(Query (bindingVars, terms')) = do
  -- ...If aliases are enabled
  terms <- expandAliases terms'
  result <- typecheckGoal terms
  let bodyVars = collectTypedBliCommandVars cmd 
  let lambdaError = 
        if (bindingVars `subset` bodyVars)
        then Right Ok
        else Left $ [BoundVarNotInBody]
  return $ joinErrors lambdaError result
typecheckBliCommand (Assert terms') = do
  terms <- expandAliases terms'
  typecheckGoal terms
typecheckBliCommand (AssertClause (t',ts')) = do
  t  <- expandAliasesTerm t'
  ts <- expandAliases ts'
  -- Note: We need to do more type-checking here. All free variables on the 
  -- RHS with expected type t should only be used in holes in the LHS
  -- with the same expected type.

  -- Get the indexes of all free vars in ts

  -- Match the free vars on the LHS with their expected types

  -- Make sure that each occurance of each variable on the RHS matches
  -- its expected type on the LHS.
  
  results1 <- typecheckGoal ts
  results2 <- typecheckTerm t
  return $ joinErrors results1 results2
-- Asserting schemas is not covered by typechecking.
typecheckBliCommand (AssertSchema _) = return $ Right Ok

-- | Helper function. Runs through a BliCommand, loading assertions and
--   schema declarations, but not processing queries. Does not validate.
--   Used to load a file for the purposes of typechecking.
loadBliCommand :: BliCommand -> Bli ()
loadBliCommand (Query (boundVars, goal)) = return ()
loadBliCommand (MkAlias x y) = do 
  newAlias x y
  return ()
loadBliCommand (Assert goal) = do
  newFacts $ map (\term -> (term, [])) goal
  return ()
loadBliCommand (AssertClause clause) = do
  newFacts [clause]
  return ()
loadBliCommand (AssertSchema (TypeOf termId typeId)) = do
  newEntity termId typeId
  return ()
loadBliCommand (AssertSchema (Type _ typeName)) = do
  newType typeName
  return ()
loadBliCommand (AssertSchema (DataType typeName constrs)) = do
  newDataType (typeName, constrs)
  return ()
loadBliCommand (AssertSchema (Pred _ predName argTypes _)) = do
  newRelation predName argTypes
  return ()
 
-- | typechecks an entire Bli Prolog program strictly -- i.e.
--   following a policy that definitions must precede their
--   definitions.
--   Note: This does not currently deal with module imports.
typecheckBliProgram :: BliProgram -> Bli (Either [InvalidClause] Ok)
typecheckBliProgram prog = do
    results <- mapM (\cmd -> do 
        result <- typecheckBliCommand cmd
        loadBliCommand cmd
        return result) commands
    return $ foldr joinErrors (Right Ok) results
  where commands = getCommands prog
