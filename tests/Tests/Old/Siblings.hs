{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

--
-- Old group of tests from pure-prolog
--

module Tests.Old.Siblings (siblings_test) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Bli.Prolog.Ast
import Bli.App.Config
import Bli.Prolog.TH
import Bli.Prolog.Parser
import Bli.Prolog.Interp
import Bli.Prolog.Interp.Data
import Bli.Prolog.SearchStrategies
import Data.List (sort)
import System.IO.Unsafe
import Control.Monad.Bli

initCmd command = initBli (AppConfig { options = (startOptions "tests"), version = "tests" }) command

queryFile filename goal search = do
  clauses <- clausesFromFile filename
  case (clauses, goal) of
    (Right p, Right g) -> do
       tree <- initCmd $ do
                 setFacts p
                 makeReportTree g
       return $ Right $ search tree
    (Left err, _) ->
      return $ Left err
    (_, Left err) ->
      return $ Left err

fromRight (Right x) = x
fromCmd (T_QueryMode goal) = goal
goal'' = fromCmd $ [bli| sibling(homer, X). |]
siblings = queryFile "./tests/pl_source_files/siblings.pl" goal''
testGoal = goal''
clauses = unsafePerformIO $ clausesFromFile "./tests/pl_source_files/siblings.pl"
result = bfs $ initCmd $ do
           setFacts clauses
           makeReportTree (fromRight testGoal)

extractSimples = map name . filter isSimple
  where name (Comp (Identifier name) _) = name
        isSimple (Comp (Identifier name) []) = True
        isSimple _              = False

massage (Right solutions) = Right $ sort $ concatMap gentleRub solutions
  where gentleRub (Solution bindings) =
          extractSimples $ map snd bindings
massage (Left err) = Left err

expected = sort [ "homer", "herb", "abbie", "homer", "jay"]

siblings_test = testGroup "Siblings"
  [ testCase "DFS" $ do
      result <- siblings dfs
      Right expected @=? massage result
  , testCase "BFS" $ do
      result <- siblings bfs
      Right expected @=? massage result
  ]