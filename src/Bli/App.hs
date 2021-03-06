
--
-- | Core application logic for bli-prolog.
--

module Bli.App where

import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Schema
import Bli.App.Config.Features
import Bli.Prolog.Parser
import Bli.Prolog.Interp
import Bli.Prolog.Interp.Data
import Bli.Prolog.SearchStrategies
import Bli.Prolog.Typechecking hiding (InvalidClause(..))
import qualified Bli.Prolog.Typechecking.Data as Typechecking
import Control.Monad.Bli.IORef
import Control.Monad.Bli.Common (ProcContainer(..))
import Control.Monad.Bli.Conversions (liftIORefFromPure)
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
    head <- ifEnabledThenElse Aliases 
              (expandAliasesTerm head') 
              (return head')
    body <- ifEnabledThenElse Aliases 
              (expandAliases body') 
              (return body')
    let clause = (head, body) 
    case tryInsert clause clauses of
        Left _ -> return $ AssertionFail AlreadyAsserted 
        Right result -> do setFacts result
                           return $ AssertionSuccess GenericAssertionSuccess

processTypecheckedBliCommand :: BliCommand -> Bli BliResult
processTypecheckedBliCommand command = do
  opts      <- getConfig
  clauses   <- getFacts
  types     <- getTypes 
  relations <- getRelations
  entities  <- getEntities
  aliases   <- getAliases
  procs     <- getProcs
  -- Helper function to check if an identifier represents a procedure.
  let isProc x = 
        case BliSet.lookup (\(ProcContainer (y,_,_)) -> x==y) procs of
          Just _  -> True
          Nothing -> False
  -- Helper function to check if an identifier represents a procedure,
  -- and if it does, returns the stored procedure itself, as well
  -- as the types of its arguments.
  let getProc x =
        case BliSet.lookup (\(ProcContainer (y,_,_)) -> x==y) procs of
          Just (ProcContainer (_,argTypes,procedure)) -> Just (argTypes, procedure)
          Nothing -> Nothing
  case command of 
    (Assert goal') -> do
       -- First, expand all aliases 
       goal <- ifEnabledThenElse Aliases 
                 (expandAliases goal')
                 (return goal')

       results <- checkForTypePredicateAssertion goal

       let ok results = results /= []

       if ok results
       then do
         -- Note: This should be an AddedEntityLocally result,
         -- but we can return mutliple results here, so I need
         -- to figure out how to handle that.
         return $ AssertionSuccess GenericAssertionSuccess
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
                   return $ AssertionSuccess GenericAssertionSuccess
           -- We can probably refine this to get it to tell us which of the terms
           -- was already asserted.
           -- If there were any errors, the assertions fails.
           Left _ -> return $ AssertionFail AlreadyAsserted
    (AssertClause (term, []) ) -> do
       let goal = [term]
       results <- checkForTypePredicateAssertion goal
       let ok results = results /= []
       if ok results
       then do
         liftIO $ print results -- debugging
         let Comp (Identifier typeId) [Comp (Identifier entityId) []] = term
         return $ AssertionSuccess $ AddedEntityLocally entityId typeId
       else assertClause term []
    (AssertClause (head,body) ) -> do
      assertClause head body
    (Query (vars, goal)) -> do
      tree <- makeReportTree goal
      let searchF = searchFunction (search opts) $ depth opts
      return $ QuerySuccess $ QueryFinished $ 
                map Solution 
                  $ map (filter (\(x,y) -> x `elem` vars)) 
                  $ map (\(Solution x) -> x) $ searchF tree
    -- Handle procedures
    (Query (_, [Comp (Identifier id) args]))
       | isProc id -> do
         -- liftIO $ putStrLn "This is a procedure."
         -- Special handling for putStrLn as a proof of concept.
         -- This won't be needed later.
         if id == "putStrLn"
         then do
           let (Comp (StringLiteral s) []) = head args
           liftIO $ putStrLn s
           return $ QuerySuccess (QueryFinished [ProcReturn])
         else return $ QuerySuccess (QueryFinished [])
    (Query (_,goal')) -> do
       -- First, expand all aliases (if aliases have been enabled)
       goal <- ifEnabledThenElse Aliases 
                 (expandAliases goal')
                 (return goal')
       let limiting lst = 
            case limit opts of
              Nothing -> lst
              Just n  -> take n lst
       let searchF = searchFunction (search opts) $ depth opts
       tree <- makeReportTree goal
       let solutions = limiting $ searchF tree
       return $ QuerySuccess (QueryFinished solutions)
    (AssertSchema schemaEntry) -> do
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
                 Left _       -> return $ AssertionFail AlreadyAsserted
                 Right result -> do
                     setRelations result
                     return $ (AssertionSuccess GenericAssertionSuccess)
          else return $ AssertionFail (TypeNotDeclared (head typesNotInSchema))
        Type _ typeName -> do
          -- Add type to schema if not in schema.
          case tryInsert typeName types of
              Left _ -> return $ AssertionFail AlreadyAsserted
              Right result -> do
                  setTypes result
                  return $ AssertionSuccess GenericAssertionSuccess
        TypeOf termId typeId -> do
          addEntityToSchema termId typeId
        DataType typeName constrs -> do
          result <- newDataType (typeName, constrs)
          case result of      
            False -> do
              -- TODO: Make this more specific
              return $ AssertionFail AlreadyAsserted
            True -> do
              return $ AssertionSuccess GenericAssertionSuccess

-- | Check to see if the user is asserting a type predicate
checkForTypePredicateAssertion :: Goal -> Bli [BliResult]
checkForTypePredicateAssertion goal = do
       typePredicates <- getTypePredicates goal
       if typePredicates == goal 
       then do
         results <- mapM (\(Comp (Identifier typ) [Comp (Identifier x) []]) -> 
                             addEntityToSchema x typ) goal
         -- if everything went successfully...
         return $ [AssertionSuccess GenericAssertionSuccess]
       else return []

processBliCommand :: BliCommand -> Bli [BliResult]
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
    Left (Typechecking.AtomsNotInSchema atoms) -> do
      case isAssertion command of
        True  -> do
          return $ [AssertionFail $ AtomsNotInSchema atoms]
        False -> do
          return $ [QueryFail $ AtomsNotInSchema atoms]
    Left Typechecking.BoundVarNotInBody -> do
      -- This can only occur for lambda queries.
      return $ [QueryFail BoundVarNotInBody]
    Left (Typechecking.NotAPredicate (x,y,z)) -> do
      case isAssertion command of
        True  -> do
          return $ [AssertionFail $ NotAPredicate [(x,y,z)]]
        False -> do
          return $ [QueryFail $ NotAPredicate [(x,y,z)]]
    Left (Typechecking.TypeError (x,n,y,z)) -> do
      case isAssertion command of
        True  -> do
          return $ [AssertionFail $ TypeError [(x,n,y,z)]]
        False -> do
          return $ [QueryFail $ TypeError [(x,n,y,z)]]
    Left (Typechecking.EntityNotDeclared x t) -> do
      case isAssertion command of
        True -> do
          -- Check to see if we are trying to assert type predicates,
          -- in which case we do not need to typecheck those terms.
          case command of
            (Assert goal) -> do
              typePredicates <- getTypePredicates goal
              case typePredicates == goal of
                True -> do
                  -- Asserting type predicates -- this is fine.
                  (\x -> [x]) <$> processTypecheckedBliCommand command
                False -> return $ [AssertionFail $ EntityNotDeclared x t]
            (AssertClause (term,[])) -> do
              let goal = [term]
              typePredicates <- getTypePredicates goal
              case typePredicates == goal of
                True -> do
                  -- Asserting type predicates -- this is fine.
                  (\x -> [x]) <$> processTypecheckedBliCommand command
                False -> return $ [AssertionFail $ EntityNotDeclared x t]
            _ -> do
              return $ [AssertionFail $EntityNotDeclared x t]
        False -> do
          return $ [QueryFail $ EntityNotDeclared x t]
    Left (Typechecking.TypeNotDeclared x) -> do
      -- This should only occur for assertions.
      return $ [AssertionFail $ TypeNotDeclared x]
    Right Ok -> do
      (\x -> [x]) <$> processTypecheckedBliCommand command

-- | Add term to schema if type is already in schema, and term not already in schema.
addEntityToSchema :: String -> String -> Bli BliResult
addEntityToSchema termId typeId = do
  types <- getTypes
  entities <- getEntities
  case BliSet.lookup (==typeId) types of
                Nothing -> return $ AssertionFail (TypeNotDeclared typeId)
                Just _ -> 
                  case tryInsert (termId, typeId) entities of
                    Left _ -> return $ AssertionFail AlreadyAsserted
                    Right result -> do
                      setEntities result
                      -- TODO: Need to update this with Bedelibry logic
                      return $ (AssertionSuccess GenericAssertionSuccess)

        