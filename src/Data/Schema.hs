
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

-- | Flag that determines if a relation is stored or not.
data IsStored = 
   Stored 
 | NotStored deriving(Eq, Show)

-- | For our typed schema entry, we can either declare that a predicate
--   with a given identity can take arguments of the supplied types,
--   we can declare a new type, or, we can declare new entities of 
--   a given type.
--
data TypedSchemaEntry = 
-- | A declaration that adds a new relation of a given type to the schema.
    Pred IsStored String [String]
-- | A declaration that adds a new type to the schema.
  | Type   String
-- | A declaration that adds a new entity of a given type to the schema. 
  | TypeOf String String
-- | An import statement for Bli Prolog.
  | Using String deriving(Eq, Show)

-- Note: We should probably refactor these all into newtypes, incase we
-- want to make use of smart construtors later.
type TypeDecl   = String
type EntityDecl = (String, String)
type RelDecl    = (String, [String])

type TypedSchema = [TypedSchemaEntry]


-- | Helper function for converting from the new format for schemas to the old format.
getArities :: [TypedSchemaEntry] -> [SchemaEntry]
getArities [] = []
getArities ((Pred _ x ts):xs)  = (x, length ts):(getArities xs)
getArities ((Type x):xs)     = getArities xs
getArities ((TypeOf _ _):xs) = getArities xs