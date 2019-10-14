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

data BliPrologType = 
   Entity 
 -- A user declared type, such as "person".
 | DeclaredType String
 -- Type of types, "type".
 | TypTypes
 | Predicate [BliPrologType]
-- Note: Is there really a difference between predicates and goals?
 | Goal [BliPrologType]
 -- A type for rules: This allows for an interesting
 -- design choice, where we allow for "first-class rules.",
 -- and so predicates are allowed to talk about rules.
 | Rule 
 | StringLit
 | IntLit
 | DateTimeLit
 -- A polymorphic list datatype
 | List BliPrologType
 | DateLit deriving(Eq)

instance Show BliPrologType where
  show Entity = "entity"
  show (DeclaredType str) = str
  show TypTypes = "type"
  show (Predicate types) = "rel[" ++ (intercalate ", " (map show types)) ++ "]"
  show (Goal types) = "goal[" ++ (intercalate ", " (map show types)) ++ "]"
  show Rule = "rule"
  show StringLit = "string"
  show IntLit = "int"
  show DateTimeLit = "datetime"
  show (List t) = "list["++ show t ++"]" 
  show DateLit = "date"

-- Subtyping relation.
infixr 9 <:
(<:) :: BliPrologType -> BliPrologType -> Bool
(<:) _ TypTypes = True
(<:) _ _ = undefined

-- Note: It is important to make a distinction here
-- between atoms and strings, because atoms
-- have already been parsed, and hence, all of the
-- validation logic is done, and this can be a total function.

-- | Helper function to return the type of an atom which has already been parsed.
typeOfAtom :: Atom -> Bli (Maybe BliPrologType)
typeOfAtom string 
  -- A variable has no type.
  | isUpper (head string)            = return $ Nothing
  | all id $ (map isNumber) $ string = return $ Just IntLit
  | head string == '"'               = return $ Just StringLit
  -- Todo: Check for date-time and date literals.
  | otherwise = do
      -- Check to see if the literal has a user-declared
      -- type
      types    <- getTypes
      entities <- getEntities
      let entityLookup = BliSet.lookup (\(a,b) -> string==a) entities
      return $ case entityLookup of
        Just (_,typeOfX) -> Just $ DeclaredType typeOfX 
        Nothing ->  do
          -- Check to see if the literal is in fact a type.
          let typeLookup = BliSet.lookup (==string) types
          case typeLookup of
            Just _ -> Just $ TypTypes
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
   | AtomsNotInSchema [Atom]
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
typecheckTerm (Comp p xs) = do
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
    Just (_, expectedTypes) -> do
      -- Helper function
      let termHead (Var x) = x
          termHead (Comp x _) = x
      -- Typecheck each of the individual arguments 
      -- of the predicate.
      let intermediateResults = map 
            (\(expectedType, x, n) -> 
             -- Find the type of x in the entity store
             -- Note: I should write a generic typeOf function,
             -- with an abstract representation of the types of terms
             -- that I can return from this function.
             case BliSet.lookup (\(a,b) -> x==a) entities of
               -- Ignore variables in type-checking.
               Nothing | isUpper (head x) -> Right Ok
                       | otherwise -> Left $ EntityNotDeclared x expectedType
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
  results1 <- typecheckGoal ts
  results2 <- typecheckTerm t
  return $ joinErrors results1 results2
-- Asserting schemas is not covered by typechecking.
typecheckBliCommand (T_AssertSchema _) = return $ Right Ok
