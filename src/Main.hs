{-# LANGUAGE DeriveDataTypeable #-}

--
-- | Main entrypoint for the bli-prolog executable.
--

module Main where

import System.Console.CmdArgs as CA hiding (program)

import Data.Prolog.Ast
import qualified Prolog.Parser as P
import qualified Prolog.Interp as I
import qualified Prolog.Analysis as A
import Control.Monad (when)
import Data.List (intersperse, isPrefixOf)
import System.Console.Readline
import Data.List.Split
import Data.Schema
import Schema.Parser
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

data Search = DFS | BFS | Limited
            deriving (Show, Eq, Data, Typeable)
instance Default Search where
  def = DFS

searchFunction DFS _     = I.dfs
searchFunction BFS _     = I.bfs
searchFunction Limited n = I.limitedDfs n

-- For JSON formatting
-- solutionToJSON :: Solution -> JSON
-- solutionToJSON = undefined

-- Help scree to print when :h is called in the REPL
replHelpScreen = foldr1 (\x -> \y -> x ++ "\n" ++ y) $
  ["Commands: "
  ,"  \27[36m:h\27[37m      Prints this help screen."
  ,"  \27[36m:exit\27[37m    Exits bli-prolog."
  ,"  \27[36m:export\27[37m  Exports the definitions stored in bli-prolog's in-memory fact store as assertions"
  ,"           to a file."
  ,"  \27[36m:load\27[37m    Loads a schema or a prolog file into bli-prolog's in-memory store."
  ,""
  ,"Usage:"
  ,"  [PROLOG_TERM|PROLOG_CLAUSE]!   Assert a fact or rule."
  ,"  [PROLOG_TERM].                 Make a standard prolog query."
  ,"  \\[FREE_VARS]. [PROLOG_TERM].   Make a lambda query."
  ,""
  ,"For more information, please see the documentation at https://github.com/Sintrastes/bli-prolog."]
data Options =
  Options { search    :: Search
          , program   :: FilePath
          , schema    :: FilePath
          , goal      :: String
          , limit     :: Maybe Int
          , depth     :: Int
          , verbose   :: Bool
          , json      :: Bool
          , server    :: Bool
          , port      :: Maybe Int
          }
  deriving (Show, Data, Typeable)

startOptions =
  Options { search = def &= help "Specify wether to use DFS, BFS, or Limited"
          , program = def &= typFile &= help "Prolog file with clauses"
          , schema = "" &= typFile &= help "Schema file"
          , limit = def &= help "Limit the number of solutions found"
          , depth = 100 &= help "Maximum depth to traverse when using limited search"
          , goal = def &= args &= typ "GOALSTRING"
          , verbose = True &= help "Specify whether or not to use verbose output (on by default)"
          , json = False &= help "Specify whether or not json output formatting is used for queries."
          , server = False &= help "Starts a REST server for processing bli prolog queries if set."
          , port = Nothing &= help "Port number to start the server."
          }
  &= summary "bli-prolog interpreter v0.3, (C) Nathan Bedell 2019"

processUserInput :: String -> Options -> Clauses -> Schema -> IO (Maybe (Either Goal Clause))
processUserInput input opts clauses schema = do
          let command = P.parseBliCommand input
          case command of   
              Right x@(AssertMode goal) -> do
                   case A.isBliCommandValid x schema of
                     Right A.Ok -> do
                       putStrLn $ "\27[32m"++"OK."++"\27[37m"++" Assertion successful."
                       return $ Just $ Left goal
                     Left (A.AtomsNotInSchema atoms) -> do
                       putStrLn $ "\27[31m"++"Failure."++"\27[37m"++" Assertion unsuccessful."
                       putStrLn $ "    The identifiers "++ show atoms
                       putStrLn $ "    have not been declared in a schema."
                       return Nothing
              Right x@(AssertClause clause) -> do
                   case A.isBliCommandValid x schema of
                     Right A.Ok -> do
                       putStrLn $ "\27[32m"++"OK."++"\27[37m"++" Assertion successful."
                       return $ Just $ Right clause
                     Left (A.AtomsNotInSchema atoms) -> do
                       putStrLn $ "\27[31m"++"Failure."++"\27[37m"++" Assertion unsuccessful."
                       putStrLn $ "    The identifiers "++ show atoms
                       putStrLn $ "    have not been declared in a schema."
                       return Nothing
              Right x@(LambdaQuery (vars, goal)) -> do
                  case A.isBliCommandValid x schema of
                    Right A.Ok -> do
                      let t = I.makeReportTree clauses goal
                      print $ map I.Solution 
                            $ map (filter (\(x,y) -> x `elem` vars)) 
                            -- Note: This is currently fixed to use bfs.
                            $ map (\(I.Solution x) -> x) $ I.bfs t
                      return Nothing
                    Left (A.AtomsNotInSchema atoms) -> do
                      putStrLn $ "\27[31m"++"Failure."++"\27[37m"++" Query unsuccessful."
                      putStrLn $ "    The identifiers "++ show atoms
                      putStrLn $ "    have not been declared in a schema."
                      return Nothing
                    Left (A.BoundVarNotInBody) -> do
                      putStrLn $ "\27[31m"++"Failure."++"\27[37m"++" Query unsuccessful."
                      putStrLn $ "    Variables bound by a lambda abstraction that do not appear"
                      putStrLn $ "    In the body of a query."
                      return Nothing
              Right x@(QueryMode goal) -> do
                   case A.isBliCommandValid x schema of
                     Right A.Ok -> do
                       let limiting lst = case limit opts of
                             Nothing -> lst
                             Just n  -> take n lst
                       let searchF = searchFunction (search opts) $ depth opts
                       let t = I.makeReportTree clauses goal
                       let solutions = limiting $ searchF t
                       case solutions of
                         [] -> do
                            putStrLn ("\27[33m"++"No solutions."++"\27[37")
                            return Nothing
                         (x:[]) -> do
                            if (show x == "true")
                            then do 
                                 putStrLn ("\27[32m"++"True."++"\27[37")
                                 return Nothing
                            else return Nothing
                         _  -> do
                            mapM_ print solutions
                            return Nothing
                     Left _ -> do
                       putStrLn $ "\27[31m"++"Invalid query."++"\27[37m"++" "
                       return Nothing
              Left err -> do putStrLn ("\27[31m"++"Error"++"\27[37m"++" parsing query string:")
                             putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                               (map (\x -> "  " ++ x)) $ 
                                               (splitOn "\n" $ show err)
                             putStrLn $ "\27[33m"++"All bli prolog commands end with either a '.' or an '!'."++"\27[37m"
                             return Nothing

repl :: Options -> Clauses -> Schema -> IO ()
repl opts clauses schema = do
  maybeLine <- readline ("\27[36m"++"?- "++ "\27[37m")
  case maybeLine of
    Nothing -> repl opts clauses schema
    Just line -> do
      case line of 
        ":h"   -> do 
          putStrLn replHelpScreen
          repl opts clauses schema
        ":exit" -> return ()
        _ | isPrefixOf ":load" line -> do
               putStrLn "\27[33mLoad command not implemented.\27[37m"
               repl opts clauses schema
          | isPrefixOf ":export" line -> do
               putStrLn "\27[33mExport command not implemented.\27[37m"
               repl opts clauses schema
          | otherwise -> do
                      response <- processUserInput ("?- "++line) opts clauses schema
                      case response of
                        Nothing -> repl opts clauses schema
                        Just (Left goal) -> (repl opts (clauses ++ (map (\term -> (term,[])) goal) ) schema)
                        Just (Right clause) -> (repl opts (clauses ++ [clause]) schema)

main = do
  -- opts <- checkOptions
  opts <- cmdArgs startOptions
  -- If file not specified, start with an empty set of clauses.
  p <- case program opts of
    "" -> return $ Right []
    _  -> P.clausesFromFile $ program opts
  s <- case schema opts of
    "" -> return $ Right []
    _  -> schemaFromFile $ schema opts
  case (p,s) of 
    (Left err,_) -> do putStrLn ("\27[31m"++"Error"++"\27[37m"++" parsing prolog file:") 
                       putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                         (map (\x -> "  " ++ x)) $ 
                                         (splitOn "\n" $ show err)
    (_,Left err) -> do putStrLn ("\27[31m"++"Error"++"\27[37m"++" parsing schema file:") 
                       putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                         (map (\x -> "  " ++ x)) $ 
                                         (splitOn "\n" $ show err)
    (Right clauses, Right schema) ->
      case goal opts of
        "" -> do
           if (verbose opts)
           then do
             putStrLn ""
             putStrLn "  |      |            |"
             putStrLn "  |      |  .         |"
             putStrLn "  |---|  |     |---|  |"
             putStrLn "  |   |  |  |  |   |  |"
             putStrLn "  |---|  |  |  |---|  |"
             putStrLn "               |"
             putStrLn "               |"
             putStrLn "Welcome to the bli-prolog interpreter v0.3! (C) Nathan Bedell 2019"
             putStrLn "Type \27[36m:h\27[37m for help, or \27[36m:exit\27[37m to quit."
           else return ()
           repl opts clauses schema
        input -> processUserInput input opts clauses schema >> return ()