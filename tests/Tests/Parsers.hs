
-- | Unit tests for our parsers
--

module Tests.Parsers where

import Test.Tasty
import Test.Tasty.HUnit
import Prolog.Parser

parser_tests = testGroup "Parsers" 
  [
    testCase ".bpl comments" $ do
      let siblings1 = undefined
      let siblings2 = undefined
      Right $ siblings1 @=? siblings2
  , testCase ".bpl parsing" $ do
      let expected = undefined
      let parsedFile = undefined
      Right $ expected @=? parsedFile
  , testCase ".pl parsing" $ do
      let expected = undefined
      let parsedFile = undefined
      Right $ expected @=? parsedFile      
  , testCase ".pl comments" $ do
      let file1 = undefined
      let file2 = undefined
      Right $ file1 @=? file2     
  , testCase ".bsch parsing" $ do
      let expected = undefined
      let parsedFile = undefined
      Right $ expected @=? parsedFile      
  , testCase ".bsch comments" $ do
      let file1 = undefined
      let file2 = undefined
      Right $ file1 @=? file2
  ]