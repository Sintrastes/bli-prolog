
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
import Data.List
import Debug.Trace

-- | New helper function for our refactoring
--   Note: To ensure for a consistent API
--   and to best allow reuse between
--   the cli and server interfaces, this
--   function should also update the state of
--   the running Bli instance.
processBliCommand :: BliCommandTyped -> Pure.Bli BliResult
processBliCommand x = do
  opts <- Pure.getOpts
  clauses <- Pure.getProgram
  schema  <- Pure.getSchema
  case x of
    (T_AssertMode goal) -> do
         case isBliCommandValid x schema of
           Right Ok -> do
               if any (\term -> not $ (term, []) `elem` clauses) goal
               then do Pure.modifyProgram 
                         (\clauses -> clauses ++
                            (map (\term -> (term,[])) goal))
                       return $ Result_AssertionSuccess
               else return $ Result_AssertionFail_AlreadyAsserted
           Left (AtomsNotInSchema atoms) ->
               return $ Result_AssertionFail atoms
           Left (WrongArities xs) -> return $ Result_QueryFail_WrongArities xs
    (T_AssertClause clause) -> do
         case isBliCommandValid x schema of
           Right Ok -> do
               if clause `elem` clauses
               then return $ Result_AssertionFail_AlreadyAsserted
               else do Pure.modifyProgram (\clauses -> clauses ++ [clause]) 
                       return $ Result_AssertionSuccess
           Left (AtomsNotInSchema atoms) ->
               return $ Result_AssertionFail atoms
           Left (WrongArities xs) -> return $ Result_QueryFail_WrongArities xs
    (T_LambdaQuery (vars, goal)) -> do
        case isBliCommandValid x schema of
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
    (T_QueryMode goal) ->
        case isBliCommandValid x schema of
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
            Pred predName argTyped -> do
                io $ putStrLn "Adding predicate to schema if not in schema."
                -- ...
            Type typeName -> do
                io $ putStrLn "Adding type to schema if not in schema."
                -- ...
            TypeOf termId typeId -> do
                io $ "Adding term to schema if type is already in schema, and term not already in schema."
                -- ...
        -- Old logic:
        -- if schemaEntry `elem` schema
        -- then return $ Result_AssertionFail_AlreadyAsserted
        -- else do Pure.modifySchema (\x -> x ++ [schemaEntry])
        --        return $ Result_AssertionSuccess