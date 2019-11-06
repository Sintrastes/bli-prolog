
module Tests.Aliases where

import Test.Tasty
import Test.Tasty.HUnit

import Bli.Prolog.TH
import Bli.App.Cli
import Bli.App.Api
import Control.Monad.Bli
import Bli.App.Config
import Bli.App

initCmd command = initBli (AppConfig { options = (startOptions "tests"), version = "tests" }) command

alias_tests = testGroup "Aliases"
  [
   -- Note: I need to refactor processCliInput so this
   -- produces more meaningful output which I can test.
    testCase "Make alias (1)" $ do
      let command = processBliCommand [bli| type person. |]
                 >> processBliCommand [bli| nate: person. |]
                 >> processCliInput ":alias nate me"
      result <- initCmd command
      undefined @=? (result :: ())
  , testCase "Make alias (2)" $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| nate: person. |]
                  >> processCliInput " :alias me nate "
       result <- initCmd command
       undefined @=? (result :: ())
  , testCase "Use alias in predicate (1)" $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| rel p: person. |]
                  >> processCliInput   ":alias me nate "
                  >> processBliCommand [bli| p(me). |]
       result <- initCmd command
       undefined @=? (head result :: BliResult)
  , testCase "Use alias in predicate (2)" $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| rel p: person. |]
                  >> processCliInput ":alias nate me"
                  >> processBliCommand [bli| p(me). |]
       result <- initCmd command
       undefined @=? (head result :: BliResult)
  , testCase "Use alias in predicate (assertion and query)" $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| rel p: person. |]
                  >> processCliInput    ":alias me nate"
                  >> processBliCommand [bli| p(me)! |]
                  >> processBliCommand [bli| p(nate). |]
       result <- initCmd command
       undefined @=? (head result :: BliResult)
  , testCase "Use aliases in predicate." $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| type animal. |]
                  >> processBliCommand [bli| rel has_pet: person, animal. |]
                  >> processBliCommand [bli| nate: person. |]
                  >> processBliCommand [bli| ninja: animal. |]
                  >> processCliInput " :alias me nate "
                  >> processCliInput " :alias my_cat ninja "
                  >> processBliCommand [bli| has_pet(me, X). |]
       result <- initCmd command
       undefined @=? (head result :: BliResult)
  , testCase "Use alias in predicate with rule. " $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| type animal. |]
                  >> processBliCommand [bli| rel has_pet: person, animal. |]
                  >> processBliCommand [bli| rel cat: animal. |]
                  >> processBliCommand [bli| rel has_cat: person, animal. |]
                  >> processBliCommand [bli| has_cat(Person, Pet) :- has_pet(Person, Pet), cat(Pet)! |]
                  >> processBliCommand [bli| nate: person. |]
                  >> processBliCommand [bli| ninja: animal. |]
                  >> processCliInput " :alias me nate "
                  >> processCliInput " :alias my_cat ninja"
                  >> processBliCommand [bli| has_cat(me, X). |]
       result <- initCmd command
       undefined @=? (head result :: BliResult)
  ]