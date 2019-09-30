
--
-- | Basic data types for dealing with blipl schemas, may also put 
--   some utility functions in this module for dealing with schemas
--   if needed.
--

module Data.Schema where

-- | A schema (curretly) is a list of identifiers,
--   together with their arity. In the future,
--   this will be a more complex datatype.
type Schema = [SchemaEntry]

prettyShowSchemaEntry (id, n) = id ++ ": " ++ show n ++ "."

type SchemaEntry = (String, Int)

-- | For our typed schema entry, we can either declare that a predicate
--   with a given identity can take arguments of the supplied types,
--   we can declare a new type, or, we can declare new entities of 
--   a given type.
--
data TypedSchemaEntry = 
    Pred String [String]
  | Type   String
  | TypeOf String String