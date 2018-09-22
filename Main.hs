{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs as CA hiding (program)

import Ast
import qualified Parser as P
import qualified Interp as I
import qualified Analysis as A
import Control.Monad(when)
import Data.List(intersperse)



data Search = DFS | BFS | Limited
            deriving (Show, Eq, Data, Typeable)
instance Default Search where
  def = DFS

searchFunction DFS _     = I.dfs
searchFunction BFS _     = I.bfs
searchFunction Limited n = I.limitedDfs n

data Analysis = Interface
              | Uses
              | External
            deriving (Show, Eq, Data, Typeable)


data Options =
  Options { search   :: Search
          , program  :: FilePath
          , goal     :: String
          , limit    :: Maybe Int
          , depth    :: Int
          , info     :: [Analysis]
          }
  deriving (Show, Data, Typeable)

startOptions =
  Options { search = def &= help "Specify wether to use DFS, BFS, or Limited"
          , program = def &= typFile &= help "Prolog file with clauses"
          , limit = def &= help "Limit the number of solutions found"
          , depth = 100 &= help "Maximum depth to traverse when using limited search"
          , info = def &= help "Don't interpret program, only analyse it"
          , goal = def &= args &= typ "GOALSTRING"
          }
  &= summary "Pure Prolog Interpreter v0.1, (C) Ken Friis Larsen 2012-2018"


checkOptions = do
  opts <- cmdArgs startOptions
  case program opts of
    "" -> error "You must provide a clauses file with the -p flag"
    _ -> case goal opts ++ (concat $ map show $ info opts) of
      "" -> error "You must provide a goal to prove or ask for an analysis"
      _ -> return opts

analyse prog Interface = do
  mapM_ putStrLn $ A.interface prog
analyse prog Uses = do
  mapM_ putStrLn $ A.uses prog
analyse prog External = do
  mapM_ putStrLn $ A.external prog

main = do
  opts <- checkOptions
  p <- P.clausesFromFile $ program opts
  case info opts of
    [] -> do
      let g = P.goalFromString $ goal opts
      let limiting lst = case limit opts of
            Nothing -> lst
            Just n  -> take n lst
      case (p, g) of
        (Right p, Right g) -> do
          let searchF = searchFunction (search opts) $ depth opts
              t = I.makeReportTree p g
              solutions = limiting $ searchF t
          case solutions of
            [] -> putStrLn "no solutions"
            _  -> mapM_ print solutions
        (Left err, _) -> error $ show err
        (_, Left err) -> error $ show err
    analysis -> do
      case p of
        Right p  -> sequence_ $ intersperse (putStrLn "") $
                      map (analyse p) analysis
        Left err -> error $ show err
