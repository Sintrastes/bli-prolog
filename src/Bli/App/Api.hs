
-- | Datatypes used for representing the different kinds of 
--   commands that can be requested of bli prolog, and
--   the sorts of responses produced by those commands.
--
-- This API should be used by both the cli and our
-- REST API to encourage code re-use.
--

module Bli.App.Api where

import Bli.Prolog.Typechecking.Data
import Bli.Prolog.Interp.Data
import Bli.App.Config.Features

-- | A data type to model the types of 
--   requests that can be made to the server
data BliRequest = 
-- | A simple Get request to 
--   make a query and return the
--   results of that query.
     MakeQuery String
-- | Request that an assertion be made.
   | MakeAssertion String deriving(Eq, Show)

-- | A data type to model the types of responses
--   that the server can return to clients.
data BliResponse = BliResponse BliResult

data BliResult = 
    SyntaxError String
  | ExtensionNotEnabled LanguageOption 
  | QueryFail FailureMode 
  | AssertionFail FailureMode
  | QuerySuccess QuerySuccessMode
  | AssertionSuccess AssertionSuccessMode deriving(Show, Eq, Read)

data FailureMode =
    BoundVarNotInBody 
  | AlreadyAsserted
  | AtomsNotInSchema [String]
  | TypeError [(String, Int, String, String)]
  | TypeNotDeclared String
  | EntityNotDeclared String String
  | NotAPredicate [(String, Int, String)]
  | WrongArities [(String,Int,Int)]
  | CannotDeclareEntityOfBuiltinType String
  | CannotDeclaraDatatypeAsEntity deriving(Show, Eq, Read)

data AssertionSuccessMode = 
    AddedEntityLocally String String
  | AddedEntityBedelibry String String
  | GenericAssertionSuccess deriving(Show, Eq, Read)

data QuerySuccessMode = 
    QueryFinished [Solution]
  | AdditionalInputRequired deriving(Show, Eq, Read)
  
-- | Helper function to combine multiple BliResults into one.
--   Note: This should probably be of type [BliResult] -> [BliResult],
--   as not all types of BliResults can be combined
joinResults :: [BliResult] -> [BliResult]
joinResults = id