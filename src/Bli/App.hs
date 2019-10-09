
--
-- | Core application logic for bli-prolog.
--

module Bli.App where

import Data.Prolog.Ast
import Data.Schema
import Prolog.Parser
import Prolog.Interp
import Prolog.Analysis
import Control.Monad.Bli.Pure
import Bli.App.Api
import Bli.App.Config
import Data.Aeson
import Data.List.Split
import Data.BliSet
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
processBliCommand :: BliCommandTyped -> Bli BliResult
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
  isValid <- isBliCommandValid command
  case isValid of
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
    Left (TypeNotDeclared x) -> do
      case isAssertion command of
        True  -> do
          return $ Result_AssertionFail_TypeNotDeclared x
        False -> do
          return $ Result_QueryFail_TypeNotDeclared x
    Right Ok -> do
      case command of 
        (T_AssertMode goal) -> do
           -- Try inserting each of the terms individually, collecting errors
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
        (T_AssertClause clause) -> do
          case tryInsert clause clauses of
                 Left _ -> return $ Result_AssertionFail_AlreadyAsserted 
                 Right result -> do setFacts result
                                    return $ Result_AssertionSuccess
        (T_LambdaQuery (vars, goal)) ->
          let t = makeReportTree clauses goal in
                return $ Result_QuerySuccess $ 
                           map Solution 
                         $ map (filter (\(x,y) -> x `elem` vars)) 
                         -- Note: This is currently fixed to use bfs.
                         $ map (\(Solution x) -> x) $ bfs t
        (T_QueryMode goal) ->
          return $ Result_QuerySuccess (  
            let limiting lst = case limit opts of
                  Nothing -> lst
                  Just n  -> take n lst
                searchF = searchFunction (search opts) $ depth opts
                t = makeReportTree clauses goal
                solutions = limiting $ searchF t
            in solutions )
        (T_AssertSchema schemaEntry) -> do
          case schemaEntry of
            Pred predName argTypes -> do
              -- Add predicate to schema if not already in schema.
              -- TODO: Also need to check here that each of the argument types
              -- is also in the schema, otherwise, return an error.
              -- Note also that this should probably be something more specific, like a
              --   "Result_SchemaUpdateFail_AlreadyInSchema"
              case tryInsert (predName, argTypes) relations of
                  Left _       -> return $ Result_AssertionFail_AlreadyAsserted
                  Right result -> do
                      setRelations result
                      return $ Result_AssertionSuccess
            Type typeName -> do
              -- Add type to schema if not in schema.
              case tryInsert typeName types of
                  Left _ -> return $ Result_AssertionFail_AlreadyAsserted
                  Right result -> do
                      setTypes result
                      return $ Result_AssertionSuccess
            TypeOf termId typeId -> do
              -- Add term to schema if type is already in schema, and term not already in schema.
              -- TODO: Need to validate the logic in the above comment still.
              case tryInsert (termId, typeId) entities of
                  Left _ -> return $ Result_AssertionFail_AlreadyAsserted
                  Right result -> do
                      setEntities result
                      return $ Result_AssertionSuccess  

        