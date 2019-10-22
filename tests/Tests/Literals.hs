
module Tests.Literals where

literal_tests = testGroup "Literals"
  [
    testCase "String literal error." $ do
      let command = processBliCommand [bli| "test". |]
    testCase "Declare string relation (1)" $ do
      let command = processBliCommand [bli| rel p: string. |]
    testCase "Declare string relation (2)" $ do
      let command = processBliCommand [bli| type person. |]
                 >> processBliCommand [bli| rel name: person, string. |]
    testCase "String literal predicate query." $ do
      let command = processBliCommand [bli| rel p: string. |]
                 >> processBliCommand [bli| p("test"). |]
    testCase "Integer literal error." $ do
      let command = processBliCommand [bli| 42. |]
    testCase "Declare integer relation (1)" $ do
      let command = processBliCommand [bli| rel p: int. |]
    testCase "Declare integer relation (2)" $ do
      let command = processBliCommand [bli| type person. |]
                 >> processBliCommand [bli| rel age: person, int. |]
    testCase "Integer predicate." $ do
      let command = processBliCommand [bli| rel p: int. |]
                    processBliCommand [bli| p(42). |]
    testCase "Datetime literal error." $ do
      let command = processBliCommand [bli| '2019. |]
    testCase "Declare datetime relation (1)" $ do
      let command = processBliCommand [bli| rel p: datetime. |]
    testCase "Declare datetime relation (2)" $ do
      let command = processBliCommand [bli| rel born: person, date. |]
    testCase "Datetime predicate." $ do
      let command = processBliCommand [bli| type person. |]
                 >> processBliCommand [bli| rel born: person, date. |]
                 >> processBliCommand [bli| nate: person. |]
                 >> processBliCommand [bli| born(nate, '1994). |]
  ]