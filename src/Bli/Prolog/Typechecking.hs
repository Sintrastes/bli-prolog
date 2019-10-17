--
-- | Implementation of typechecking for
--   bli prolog.
--

module Bli.Prolog.Typechecking where

import Data.Bli.Prolog.Ast
import Bli.Prolog.Interp (expandAliases, expandAliasesTerm)
import Bli.Prolog.Interp.Data (isPlain)
import qualified Data.Set as S
import Data.List((\\), nub, intercalate)
import Data.Char
import Control.Monad (join)
import Data.Bli.Prolog.Schema
import Control.Monad.Bli
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
collectTypedBliCommandAtoms (T_QueryMode goal)       = nub . join . (map collectTermAtoms) $ goal
collectTypedBliCommandAtoms (T_AssertMode goal)      = nub . join . (map collectTermAtoms) $ goal
collectTypedBliCommandAtoms (T_AssertClause clause)  = collectClauseAtoms clause
collectTypedBliCommandAtoms (T_LambdaQuery (_,goal)) = nub . join . (map collectTermAtoms) $ goal
collectTypedBliCommandAtoms (T_AssertSchema _) = []

-- | Helper function. Checks to see which variables are used in a bli prolog command.
collectTypedBliCommandVars :: BliCommand -> [Variable]
collectTypedBliCommandVars (T_QueryMode goal)       = nub . join . (map collectTermVars) $ goal
collectTypedBliCommandVars (T_AssertMode goal)      = nub . join . (map collectTermVars) $ goal
collectTypedBliCommandVars (T_AssertClause clause)  = collectClauseVars clause
collectTypedBliCommandVars (T_LambdaQuery (_,goal)) = nub . join . (map collectTermVars) $ goal
collectTypedBliCommandVars (T_AssertSchema _) = []

-- Subtyping relation.
infixr 9 <:
(<:) :: BliPrologType -> BliPrologType -> Bli Bool
(<:) _ TypTypesT = return $ True
(<:) (DeclaredTypeT x) EntityT = return $ True
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

-- | Unit type, representing valid input.
data Ok = Ok

-- Note: It is important to make a distinction here
-- between atoms and strings, because atoms
-- have already been parsed, and hence, all of the
-- validation logic is done, and this can be a total function.

-- | Helper function to return the type of an atom which has already been parsed.
typeOfAtom :: Atom -> Bli (Maybe BliPrologType)
typeOfAtom (StringLiteral string) = return $ Just $ StringLitT
typeOfAtom (IntLiteral x) = return $ Just $ IntLitT
typeOfAtom (DataLit name args) = do
  -- lookup user-defined datatypes in scope,
  -- and see if our DataLit matches any of the types
  -- in our schema.
  maybeConstrType <- lookupTypeOfDataConstr name
  case maybeConstrType of
    Nothing -> return $ Nothing
    Just constrType -> return $ Just $ DeclaredTypeT constrType
typeOfAtom (AppTerm _ _) = do 
  -- 
  undefined
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
  | isUpper (head string)            = return $ Nothing
  | otherwise = do
      -- Check to see if the literal has a user-declared
      -- type
      types    <- getTypes
      entities <- getEntities
      let entityLookup = BliSet.lookup (\(a,b) -> string==a) entities
      return $ case entityLookup of
        Just (_,typeOfX) -> Just $ DeclaredTypeT typeOfX 
        Nothing ->  do
          -- Check to see if the literal is in fact a type.
          let typeLookup = BliSet.lookup (==string) types
          case typeLookup of
            Just _ -> Just $ TypTypesT
            Nothing -> Nothing

-- | Data type representing all of the possible errors
--   that can occur from validating the input to a bli prolog
--   command. 
data InvalidClause =
     -- | Error to return when a lambda query contains a bound variable which does not
     --   appear in the body.
     BoundVarNotInBody 
     -- | Error to return when a query (or an assertion) of any kind contains
     --   identifiers which do not exist in any of the imported schemas.
   | AtomsNotInSchema [String]
-- ... Identifier X is being used as an Nary predicate, but is declared to
--     be a term of type Y in the schema.
   | NotAPredicate (String, Int, String)
-- ... Encountered type errors:                                                                         
--         In predicate X, argument n is not of type Y, but rather of type W.
   | TypeError (String, Int, String, String)
-- | type X has not been declared in the schema.
   | TypeNotDeclared String
   | EntityNotDeclared String String
-- The term ... has not been declared as a relation of type ...
   | RelationNotDeclared String [String]
   | AlreadyAsserted deriving(Eq, Show)

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
      let expectedTypes = map (\x -> case x of
                                       "string"  -> StringLitT
                                       "period"  -> DateTimeLitT
                                       "int"     -> IntLitT
                                       "entity"  -> EntityT
                                       "rule"    -> RuleT
                                       otherwise -> DeclaredTypeT x)
                              expectedTypes'
      -- Helper function
      let termHead (Var x) = Identifier x
          termHead (Comp x _) = x
      -- Typecheck each of the individual arguments 
      -- of the predicate.
      intermediateResults <- mapM 
            (\(expectedType, x, n) -> do
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
      -- Return all of the errors that were encountered, or none
      -- if no errors were encountered.
      return $ collectErrors intermediateResults
typecheckTerm (Comp x xs) = do
  typeOfX <- typeOfAtom x
  return $ Left $ [NotAPredicate (show x, length xs, show typeOfX)]

typecheckGoal :: Goal -> Bli (Either [InvalidClause] Ok)
typecheckGoal terms = do
  results <- mapM typecheckTerm terms
  return $ foldr joinErrors (Right Ok) results

typecheckBliCommand :: BliCommand -> Bli (Either [InvalidClause] Ok)
typecheckBliCommand (T_QueryMode goal') = do
  goal <- expandAliases goal'
  typecheckGoal goal
typecheckBliCommand cmd@(T_LambdaQuery (bindingVars, terms')) = do
  terms <- expandAliases terms'
  result <- typecheckGoal terms
  let bodyVars = collectTypedBliCommandVars cmd 
  let lambdaError = 
        if (bindingVars `subset` bodyVars)
        then Right Ok
        else Left $ [BoundVarNotInBody]
  return $ joinErrors lambdaError result
typecheckBliCommand (T_AssertMode terms') = do
  terms <- expandAliases terms'
  typecheckGoal terms
typecheckBliCommand (T_AssertClause (t',ts')) = do
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
typecheckBliCommand (T_AssertSchema _) = return $ Right Ok
