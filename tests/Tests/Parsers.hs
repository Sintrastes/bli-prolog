
-- | Unit tests for our parsers
--

module Tests.Parsers where

import Test.Tasty
import Test.Tasty.HUnit
import Prolog.Parser

parser_tests = testGroup "Parsers" 
  [
    testCase ".bpl comments" $ do
      siblings1 <- parseBliFromFile "./tests/pl_source_files/siblings.bpl"
      siblings2 <- parseBliFromFile "./tests/pl_source_files/siblings2.bpl"
      siblings1 @=? siblings2
  , testCase "Command line improperly terminated query." $ do
      processCliInput "test.1"    
  , testCase "Command line improperly terminated assertion." $ do
      processCliInput "test!2"
  , testCase ".bpl parsing" $ do
      let expected = undefined
      let parsedFile = undefined
      expected @=? parsedFile
  , testCase ".pl parsing" $ do
      let expected = undefined
      let parsedFile = undefined
      expected @=? parsedFile       
  , testCase ".pl comments" $ do
      let file1 = undefined
      let file2 = undefined
      file1 @=? file2     
  , testCase ".bsc parsing" $ do
      let expected = undefined
      let parsedFile = undefined
      expected @=? parsedFile      
  , testCase ".bsc comments" $ do
      let file1 = undefined
      let file2 = undefined
      file1 @=? file2
  ]