
module Tests.Aliases where

import Bli.Prolog.TH

alias_tests = testGroup "Aliases"
  [
    testCase "Make alias (1)" $ do
      let command = processBliCommand [bli| type person. |]
                 >> processBliCommand [bli| nate: person. |]
                 >> processBliCommand [bli| :alias nate me |]
      result <- initCmd command
  , testCase "Make alias (2)" $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| nate: person. |]
                  >> processBliCommand [bli| :alias me nate |]
       result <- initCmd command
  , testCase "Use alias in predicate (1)" $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| rel p: person. |]
                  >> processBliCommand [bli| :alias me nate |]
                  >> processBliCommand [bli| p(me). |]
       result <- initCmd command
  , testCase "Use alias in predicate (2)" $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| rel p: person. |]
                  >> processBliCommand [bli| :alias nate me |]
                  >> processBliCommand [bli| p(me). |]
       result <- initCmd command
  , testCase "Use alias in predicate (assertion and query)" $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| rel p: person. |]
                  >> processBliCommand [bli| :alias me nate |]
                  >> processBliCommand [bli| p(me)! |]
                  >> processBliCommand [bli| p(nate). |]
       result <- initCmd command
  , testCase "Use aliases in predicate." $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| type animal. |]
                  >> processBliCommand [bli| rel has_pet: person, animal. |]
                  >> processBliCommand [bli| nate: person. |]
                  >> processBliCommand [bli| ninja: animal. |]
                  >> processBliCommand [bli| :alias me nate |]
                  >> processBliCommand [bli| :alias my_cat ninja |]
                  >> processBliCommand [bli| has_pet(me, X). |]
       result <- initCmd command
  , testCase "Use alias in predicate with rule. " $ do
       let command = processBliCommand [bli| type person. |]
                  >> processBliCommand [bli| type animal. |]
                  >> processBliCommand [bli| rel has_pet: person, animal. |]
                  >> processBliCommand [bli| rel cat: animal. |]
                  >> processBliCommand [bli| rel has_cat: person, animal. |]
                  >> processBliCommand [bli| has_cat(Person, Pet) :- has_pet(Person, Pet), cat(Pet). |]
                  >> processBliCommand [bli| nate: person. |]
                  >> processBliCommand [bli| ninja: animal. |]
                  >> processBliCommand [bli| :alias me nate |]
                  >> processBliCommand [bli| :alias my_cat ninja |]
                  >> processBliCommand [bli| has_cat(me, X). |]
       result <- initCmd command
  ]