{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs as CA hiding (program)

import Data.Prolog.Ast
import qualified Prolog.Parser as P
import qualified Prolog.Interp as I
import qualified Prolog.Analysis as A
import Control.Monad (when)
import Data.List (intersperse)
import System.Console.Readline
import Data.List.Split

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
  &= summary "bli-prolog interpreter v0.1, (C) Nathan Bedell 2019"

processGoalstring goalstring opts clauses = do
          let goal = P.goalFromString goalstring
          let limiting lst = case limit opts of
                Nothing -> lst
                Just n  -> take n lst
          case (clauses, goal) of
             (Right p, Right g) -> do
                   let searchF = searchFunction (search opts) $ depth opts
                   let t = I.makeReportTree p g
                   let solutions = limiting $ searchF t
                   case solutions of
                      [] -> putStrLn "no solutions"
                      _  -> mapM_ print solutions
             (Left err, _) -> do putStrLn "Error parsing file:" 
                                 putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                                   (map (\x -> "  " ++ x)) $ 
                                                   (splitOn "\n" $ show err)     
             (_, Left err) -> do putStrLn "Error parsing query string:" 
                                 putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                                   (map (\x -> "  " ++ x)) $ 
                                                   (splitOn "\n" $ show err)

repl opts clauses = do
  maybeLine <- readline "?- "
  case maybeLine of
    Nothing -> repl opts clauses
    Just line -> do
      case line of 
        ":h"   -> do 
          putStrLn "Help message"
          repl opts clauses
        "exit" -> return ()
        otherwise -> do
          -- Note: If it starts with :load, we should load a 
          -- schema or a knowledge base.
          processGoalstring ("?- "++line) opts clauses
          repl opts clauses

main = do
  -- opts <- checkOptions
  opts <- cmdArgs startOptions
  -- If file not specified, start with an empty set of clauses.
  p <- case program opts of
    "" -> return $ Right []
    _  -> P.clausesFromFile $ program opts
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
         putStrLn "bli-prolog interpreter v0.1, (C) Nathan Bedell 2019"
         putStrLn "Type \":h\" for help, or \"exit\" to quit."
       else return ()
       repl opts p
    goalstring -> processGoalstring goalstring opts p
    
