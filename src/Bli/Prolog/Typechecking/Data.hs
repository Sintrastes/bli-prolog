
module Bli.Prolog.Typechecking.Data where

-- | Data type representing all of the possible errors
--   that can occur from validating the input to a bli prolog
--   command. 
data InvalidClause =
     -- | Error to return when a lambda query contains a bound variable which does not
     --   appear in the body.
     BoundVarNotInBody 
     -- | Error to return when a query (or an assertion) of any kind contains
     --   identifiers which do not exist in any of the imported schemas.
   | AtomsNotInSchema [String]
-- ... Identifier X is being used as an Nary predicate, but is declared to
--     be a term of type Y in the schema.
   | NotAPredicate (String, Int, String)
-- ... Encountered type errors:                                                                         
--         In predicate X, argument n is not of type Y, but rather of type W.
   | TypeError (String, Int, String, String)
-- | type X has not been declared in the schema.
   | TypeNotDeclared String
   | EntityNotDeclared String String
-- The term ... has not been declared as a relation of type ...
   | RelationNotDeclared String [String]
   | AlreadyAsserted deriving(Eq, Show)

-- | Unit type, representing valid input.
data Ok = Ok deriving(Show)
