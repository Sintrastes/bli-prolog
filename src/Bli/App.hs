
--
-- | Core application logic for bli-prolog.
--

module Bli.App where

import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Schema
import Bli.Prolog.Parser
import Bli.Prolog.Interp
import Bli.Prolog.Interp.Data
import Bli.Prolog.SearchStrategies
import Bli.Prolog.Typechecking
import Control.Monad.Bli
import Control.Monad.Bli.Pure (liftFromPure)
import Bli.App.Api
import Bli.App.Config
import Data.Aeson
import Data.List.Split
import Data.BliSet hiding (lookup)
import qualified Data.BliSet as BliSet
import Data.List hiding (foldr1)
import Data.Foldable (foldr1)
import Debug.Trace
import Control.Monad
import Control.Monad.IO.Class

-- | New helper function for our refactoring
--   Note: To ensure for a consistent API
--   and to best allow reuse between
--   the cli and server interfaces, this
--   function should also update the state of
--   the running Bli instance.
-- Note: currently this uses the pure Bli monad, but eventually,
-- if we allow interacting with the Bli server, we might have to
-- allow impurity here.


assertClause head' body' = do
-- First, expand all aliases 
      clauses <- getFacts
      head <- expandAliasesTerm head'
      body <- expandAliases body'
      let clause = (head, body)
      case tryInsert clause clauses of
             Left _ -> return $ Result_AssertionFail_AlreadyAsserted 
             Right result -> do setFacts result
                                return $ Result_AssertionSuccess

processTypecheckedBliCommand :: BliCommand -> Bli BliResult
processTypecheckedBliCommand command = do
  opts      <- getConfig
  clauses   <- getFacts
  types     <- getTypes 
  relations <- getRelations
  entities  <- getEntities
  aliases   <- getAliases
  case command of 
    (T_AssertMode goal') -> do
       -- First, expand all aliases 
       goal <- expandAliases goal'

       results <- checkForTypePredicateAssertion goal

       let ok results = results /= []

       if ok results
       then do
         -- Note: This should be an AddedEntityLocally result,
         -- but we can return mutliple results here, so I need
         -- to figure out how to handle that.
         return Result_AssertionSuccess
       else do  -- Note: If there is a mixutre of type predicates
                -- and other predicates being asserted, I don't know
                -- what to do.
         -- Otherwise, try inserting each of the terms individually, collecting errors
         let collected = 
               foldr1 (>=>) 
                   (map (\term -> \result -> tryInsert (term, []) result) 
                        goal)
                  =<< Right clauses 
         case collected of
         -- If all is well, update the store.
           Right result -> do 
                   setFacts result 
                   return $ Result_AssertionSuccess
           -- We can probably refine this to get it to tell us which of the terms
           -- was already asserted.
           -- If there were any errors, the assertions fails.
           Left _ -> return $ Result_AssertionFail_AlreadyAsserted
    (T_AssertClause (term, []) ) -> do
       let goal = [term]
       results <- checkForTypePredicateAssertion goal
       let ok results = results /= []
       if ok results
       then do
         liftIO $ print results -- debugging
         let Comp (Identifier typeId) [Comp (Identifier entityId) []] = term
         return $ Result_AssertionSuccess_AddedEntityLocally entityId typeId
       else assertClause term []
    (T_AssertClause (head,body) ) -> do
      assertClause head body
    (T_LambdaQuery (vars, goal)) -> do
      tree <- makeReportTree goal
      let searchF = searchFunction (search opts) $ depth opts
      return $ Result_QuerySuccess $ 
                map Solution 
                  $ map (filter (\(x,y) -> x `elem` vars)) 
                  $ map (\(Solution x) -> x) $ searchF tree
    (T_QueryMode goal') -> do
       -- First, expand all aliases 
       goal <- expandAliases goal'
       let limiting lst = 
            case limit opts of
              Nothing -> lst
              Just n  -> take n lst
       let searchF = searchFunction (search opts) $ depth opts
       tree <- makeReportTree goal
       let solutions = limiting $ searchF tree
       return $ Result_QuerySuccess solutions
    (T_AssertSchema schemaEntry) -> do
      case schemaEntry of
        Pred _ predName argTypes _ -> do
          -- Add predicate to schema if not already in schema,
          -- and if each of the argument types is also in the schema,
          -- otherwise, return an error.
          let isJust (Just x) = True -- Helper function
              isJust Nothing  = False
          let typesNotInSchema = map snd $
                filter (\(x,y) -> not $ isJust x)
                  (zip (map (\x -> BliSet.lookup (==x) types) argTypes) argTypes)
          if typesNotInSchema == []
          then case tryInsert (predName, argTypes) relations of
                 Left _       -> return $ Result_AssertionFail_AlreadyAsserted
                 Right result -> do
                     setRelations result
                     return $ Result_AssertionSuccess
          else return $ Result_AssertionFail_TypeNotDeclared (head typesNotInSchema)
        Type typeName -> do
          -- Add type to schema if not in schema.
          case tryInsert typeName types of
              Left _ -> return $ Result_AssertionFail_AlreadyAsserted
              Right result -> do
                  setTypes result
                  return $ Result_AssertionSuccess
        TypeOf termId typeId -> do
          addEntityToSchema termId typeId
        DataType typeName constrs -> do
          result <- newDataType (typeName, constrs)
          case result of      
            False -> do
              -- TODO: Make this more specific
              return $ Result_AssertionFail_AlreadyAsserted
            True -> do
              return $ Result_AssertionSuccess

-- | Check to see if the user is asserting a type predicate
checkForTypePredicateAssertion :: Goal -> Bli [BliResult]
checkForTypePredicateAssertion goal = do
       typePredicates <- getTypePredicates goal
       if typePredicates == goal 
       then do
         results <- mapM (\(Comp (Identifier typ) [Comp (Identifier x) []]) -> 
                             addEntityToSchema x typ) goal
         -- if everything went successfully...
         return $ [Result_AssertionSuccess]
       else return []

processBliCommand :: BliCommand -> Bli BliResult
processBliCommand command = do
  opts      <- getConfig
  clauses   <- getFacts
  types     <- getTypes 
  relations <- getRelations
  entities  <- getEntities
  aliases   <- getAliases
  -- Note: Currently we are doing the validation logic in each one of these sub-cases,
  -- But I think it would be cleaner to do it beforehand, and then
  -- using a utiity function that checks if a command is an assertion or not if needed.
  let mapLeft f (Left x) = Left (f x)
      mapLeft f (Right x) = Right x
  typecheckResult <- typecheckBliCommand command
  -- Just deal with the first error. Later we will probably want to
  -- display multiple errors.
  case mapLeft head typecheckResult of
    Left (AtomsNotInSchema atoms) -> do
      case isAssertion command of
        True  -> do
          return $ Result_AssertionFail_AtomsNotInSchema atoms
        False -> do
          return $ Result_QueryFail_AtomsNotInSchema atoms
    Left BoundVarNotInBody -> do
      -- This can only occur for lambda queries.
      return $ Result_QueryFail_BoundVarNotInBody
    Left (NotAPredicate (x,y,z)) -> do
      case isAssertion command of
        True  -> do
          return $ Result_AssertionFail_NotAPredicate [(x,y,z)] 
        False -> do
          return $ Result_QueryFail_NotAPredicate [(x,y,z)]
    Left (TypeError (x,n,y,z)) -> do
      case isAssertion command of
        True  -> do
          return $ Result_AssertionFail_TypeError [(x,n,y,z)]
        False -> do
          return $ Result_QueryFail_TypeError [(x,n,y,z)]
    Left (EntityNotDeclared x t) -> do
      case isAssertion command of
        True -> do
          -- Check to see if we are trying to assert type predicates,
          -- in which case we do not need to typecheck those terms.
          case command of
            (T_AssertMode goal) -> do
              typePredicates <- getTypePredicates goal
              case typePredicates == goal of
                True -> do
                  -- Asserting type predicates -- this is fine.
                  processTypecheckedBliCommand command
                False -> return $ Result_AssertionFail_EntityNotDeclared x t
            (T_AssertClause (term,[])) -> do
              let goal = [term]
              typePredicates <- getTypePredicates goal
              case typePredicates == goal of
                True -> do
                  -- Asserting type predicates -- this is fine.
                  processTypecheckedBliCommand command
                False -> return $ Result_AssertionFail_EntityNotDeclared x t
            _ -> do
              return $ Result_AssertionFail_EntityNotDeclared x t
        False -> do
          return $ Result_QueryFail_EntityNotDeclared x t
    Left (TypeNotDeclared x) -> do
      -- This should only occur for assertions.
      return $ Result_AssertionFail_TypeNotDeclared x
    Right Ok -> do
      processTypecheckedBliCommand command

-- | Add term to schema if type is already in schema, and term not already in schema.
addEntityToSchema :: String -> String -> Bli BliResult
addEntityToSchema termId typeId = do
  types <- getTypes
  entities <- getEntities
  case BliSet.lookup (==typeId) types of
                Nothing -> return $ Result_AssertionFail_TypeNotDeclared typeId
                Just _ -> 
                  case tryInsert (termId, typeId) entities of
                    Left _ -> return $ Result_AssertionFail_AlreadyAsserted
                    Right result -> do
                      setEntities result
                      return $ Result_AssertionSuccess  

        