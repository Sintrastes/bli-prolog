
module Tests.App where

--
-- | Unit test cases for the main application logic
-- 

import Test.Tasty
import Test.Tasty.HUnit

import Bli.App
import Bli.App.Api
import Bli.App.Config
import Bli.App.Api
import Bli.Prolog.Typechecking
import Control.Monad
import Control.Monad.Bli
import Bli.Prolog.TH

initCmd command = initBli (AppConfig { options = (startOptions "tests"), version = "tests" }) command

app_tests = testGroup "App"
  [
    testCase "Assert schema twice" $ do
      let command = processBliCommand [bli| rel test. |]
                 >> processBliCommand [bli| rel test. |]
      result <- initCmd command
      Result_AssertionFail_AlreadyAsserted @=? result 
  , testCase "Assert fact twice" $ do
      let command = processBliCommand [bli| rel test. |]
                 >> processBliCommand [bli| test! |]
                 >> processBliCommand [bli| test! |]
      result <- initCmd command
      Result_AssertionFail_AlreadyAsserted @=? result 
  , testCase "Assert rule twice" $ do
      let command = processBliCommand [bli| rel a. |]
                 >> processBliCommand [bli| rel b. |]
                 >> processBliCommand [bli| a :- b! |]
                 >> processBliCommand [bli| a :- b! |]
      result <- initCmd command
      Result_AssertionFail_AlreadyAsserted @=? result 
  , testCase "Query undeclared predicate." $ do
      let command = processBliCommand [bli| test. |]
      result <- initCmd command
      Result_QueryFail_AtomsNotInSchema ["test"] @=? result
  -- Note: We should also test more complicated exampels here
  -- with nested predicates.
  -- Note: These examples here actually show that I still need a WrongArities
  -- result.
  , testCase "Query with wrong arity." $ do
      let command = processBliCommand [bli| rel test.  |]
                 >> processBliCommand [bli| test(X). |]
      result <- initCmd command
      Result_QueryFail_WrongArities [("test", 1, 0)] @=? result
  , testCase "Assertion with wrong arity." $ do 
      let command = processBliCommand [bli| rel test/0.  |]
                 >> processBliCommand [bli| test(X)! |]
      result <- initCmd command
      Result_AssertionFail_WrongArities [("test", 1, 0)] @=? result

  , testCase "Lambda query with wrong arity." $ do
      let command = processBliCommand [bli| rel test.  |]
                 >> processBliCommand [bli| \X. test(X). |]
      result <- initCmd command
      Result_QueryFail_WrongArities [("test", 1, 0)] @=? result
  , testCase "Bound variable not in body." $ do
      let command = processBliCommand [bli| rel test.  |]
                 >> processBliCommand [bli| \X,Y. test(X). |]
      result <- initCmd command
      Result_QueryFail_BoundVarNotInBody @=? result
  ] 