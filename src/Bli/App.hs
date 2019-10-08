
--
-- | Core application logic for bli-prolog.
--

module Bli.App where

import Data.Prolog.Ast
import Data.Schema
import Prolog.Parser
import Prolog.Interp
import Prolog.Analysis
import qualified Control.Monad.Bli.Pure as Pure
import Bli.App.Api
import Bli.App.Config
import Data.Aeson
import Data.List.Split
import Data.BliSet
import Data.List
import Debug.Trace
import Control.Monad.IO.Class

-- | New helper function for our refactoring
--   Note: To ensure for a consistent API
--   and to best allow reuse between
--   the cli and server interfaces, this
--   function should also update the state of
--   the running Bli instance.
processBliCommand :: BliCommandTyped -> Pure.Bli BliResult
processBliCommand command = do
  opts      <- Pure.getConfig
  clauses   <- Pure.getFacts
  types     <- Pure.getTypes 
  relations <- Pure.getRelations
  entities  <- Pure.getEntities
  aliases   <- Pure.getAliases
  case command of
    (T_AssertMode goal) -> do
         isValid <- isBliCommandValid command
         case isValid of
           Right Ok -> do
               -- Try inserting each of the terms individually, collecting errors
               case foldr1 (>=>) (map (\term -> \result -> tryInsert (term, []) result) goal) $ Right clauses of
               -- If all is well, update the store.
                 Right result -> do 
                         Pure.modifyFacts result 
                         return $ Result_AssertionSuccess
                 -- We can probably refine this to get it to tell us whihc of the terms
                 -- was already asserted.
                 -- If there were any errors, the assertions fails.
                 Left _ -> return $ Result_AssertionFail_AlreadyAsserted
           Left (AtomsNotInSchema atoms) -> return $ Result_AssertionFail atoms
           Left (WrongArities xs) -> return $ Result_QueryFail_WrongArities xs
    (T_AssertClause clause) -> do
         isValid <- isBliCommandValid command
         case isValid of
           Right Ok -> do
               case tryInsert clause clauses of
                 Left _ -> return $ Result_AssertionFail_AlreadyAsserted 
                 Right result -> do Pure.setFacts result
                                    return $ Result_AssertionSuccess
           Left (AtomsNotInSchema atoms) ->
               return $ Result_AssertionFail atoms
           Left (WrongArities xs) -> return $ Result_QueryFail_WrongArities xs
    (T_LambdaQuery (vars, goal)) -> do
        isValid <- isBliCommandValid command
        case isValid of
          Right Ok -> 
            let t = makeReportTree clauses goal in
              return $ Result_QuerySuccess $ 
                           map Solution 
                         $ map (filter (\(x,y) -> x `elem` vars)) 
                         -- Note: This is currently fixed to use bfs.
                         $ map (\(Solution x) -> x) $ bfs t
          Left BoundVarNotInBody ->
            return $ Result_QueryFail BoundVarNotInBody
          Left (WrongArities xs) -> return $ Result_QueryFail_WrongArities xs
          Left (AtomsNotInSchema atoms) ->
            return $ Result_QueryFail (AtomsNotInSchema atoms)
    (T_QueryMode goal) -> do
       isValid <- isBliCommandValid command 
       case isValid of
          Right Ok ->
            return $ Result_QuerySuccess (  
              let limiting lst = case limit opts of
                    Nothing -> lst
                    Just n  -> take n lst
                  searchF = searchFunction (search opts) $ depth opts
                  t = makeReportTree clauses goal
                  solutions = limiting $ searchF t
              in solutions )
          Left (AtomsNotInSchema atoms) ->
            return $ Result_QueryFail (AtomsNotInSchema atoms)
          Left (WrongArities xs) -> return $ Result_QueryFail_WrongArities xs
    -- This case should not be possible since we are not dealing with a
    -- lambda query.
          Left _ -> error $ "Invalid exception encountered."
    (T_AssertSchema schemaEntry) -> do
        case schemaEntry of
            Pred predName argTypes -> do
                liftIO $ putStrLn "Adding predicate to schema if not in schema."
                case tryInsert (predName, argTypes) relations of
                    Left _       -> return $ Result_AssertionFail_AlreadyAsserted
                    Right result -> do
                        Pure.setRelations result
                        return $ Result_AssertionSuccess
            Type typeName -> do
                liftIO $ putStrLn "Adding type to schema if not in schema."
                case tryInsert typeName types of
                    Left _ -> return $ Result_AssertionFail_AlreadyAsserted
                    Right result -> do
                        Pure.setTypes result
                        return $ Result_AssertionSuccess
            TypeOf termId typeId -> do
                liftIO $ "Adding term to schema if type is already in schema, and term not already in schema."
                case tryInsert (termId, typeId) entities of
                    Left _ -> return $ Result_AssertionFail_AlreadyAsserted
                    Right result -> do
                        Pure.setEntities result
                        return $ Result_AssertionSuccess