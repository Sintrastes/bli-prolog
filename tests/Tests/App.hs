
module Tests.App where

--
-- | Unit test cases for the main application logic
-- 

import Test.Tasty
import Test.Tasty.HUnit

import Bli.App
import Bli.App.Config
import Bli.App.Api
import Prolog.Analysis
import Control.Monad
import Control.Monad.Bli.Pure
import Data.Prolog.TemplateHaskell

app_tests = testGroup "App"
  [
    testCase "Assert schema twice" $ 
      let command = processBliCommand [bli| test: 0 |]
                 >> processBliCommand [bli| test: 0 |]
          result = runBli (startOptions "tests") [] [] command in
      Result_AssertionFail_AlreadyAsserted @=? result 
  , testCase "Assert fact twice" $
      let command = processBliCommand [bli| test: 0 |]
                 >> processBliCommand [bli| test! |]
                 >> processBliCommand [bli| test! |]
          result = runBli (startOptions "tests") [] [] command in
      Result_AssertionFail_AlreadyAsserted @=? result 
  , testCase "Assert rule twice" $
      let command = processBliCommand [bli| a: 0 |]
                 >> processBliCommand [bli| b: 0 |]
                 >> processBliCommand [bli| a :- b! |]
                 >> processBliCommand [bli| a :- b! |]
          result = runBli (startOptions "tests") [] [] command in
      Result_AssertionFail_AlreadyAsserted @=? result 
  , testCase "Query undeclared predicate." $
      let command = processBliCommand [bli| test. |]
          result = runBli (startOptions "tests") [] [] command in
      Result_QueryFail (AtomsNotInSchema ["test"]) @=? result
  -- Note: We should also test more complicated exampels here
  -- with nested predicates.
  , testCase "Query with wrong arity." $ 
      let command = processBliCommand [bli| test: 0  |]
                 >> processBliCommand [bli| test(X). |]
          result = runBli (startOptions "tests") [] [] command in
      Result_QueryFail_WrongArities [("test", 1)] @=? result
  , testCase "Assertion with wrong arity." $ 
      let command = processBliCommand [bli| test: 0  |]
                 >> processBliCommand [bli| test(X)! |]
          result = runBli (startOptions "tests") [] [] command in
      Result_AssertionFail_WrongArities [("test", 1)] @=? result

  , testCase "Lambda query with wrong arity." $
      let command = processBliCommand [bli| test: 0  |]
                 >> processBliCommand [bli| \X. test(X). |]
          result = runBli (startOptions "tests") [] [] command in
      Result_QueryFail_WrongArities [("test", 1)] @=? result
  , testCase "Bound variable not in body." $
      let command = processBliCommand [bli| test: 0  |]
                 >> processBliCommand [bli| \X,Y. test(X). |]
          result = runBli (startOptions "tests") [] [] command in
      Result_QueryFail BoundVarNotInBody @=? result
  ] 