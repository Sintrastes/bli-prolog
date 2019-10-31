
--
-- | Basic data types for dealing with blipl schemas, may also put 
--   some utility functions in this module for dealing with schemas
--   if needed.
--

module Data.Bli.Prolog.Schema where

import Language.Haskell.TH.Lift
import Data.Bli.Prolog.Types
import Bli.App.Config.Features
import Data.Serialize
import GHC.Generics

-- | Flag that determines if a relation is stored or not.
data IsStored = 
   Stored 
-- Special type of storage handled
-- by the bedebliry server.
 | External
-- Function found in the given haskell module
 | ExternalHS String
 | NotStored deriving(Eq, Show, Lift, Generic)

data FixityDirection = Infixr | Infixl deriving(Eq, Show, Lift, Generic)

instance Serialize IsStored
instance Serialize FixityDirection

-- | For our typed schema entry, we can either declare that a predicate
--   with a given identity can take arguments of the supplied types,
--   we can declare a new type, or, we can declare new entities of 
--   a given type.
--
data SchemaEntry = 
-- | A declaration that adds a new relation of a given type to the schema.
    Pred IsStored String [String] [Direction]
-- | A declaration that adds a new type to the schema.
  | Type   String
-- | A declaration that adds a new entity of a given type to the schema. 
  | TypeOf String String
-- | Declares that a binary operator should either be declared as infixl or infixr.
--   infixr $
  | Fixity FixityDirection String
-- | Declares that a binary operator has lower precedence than some other binary operator.
--   These declarations can be overidden at the module level by specifying an
--   *override* parameter. (though I'm not entirely sure how this should work yet)
--   prec $ < .
  | FixityCmp String String
-- | An import statement for Bli Prolog.
  | Using String 
-- | A user declaration that one type is a subtype of another.
  | Subtype String String
-- | A declaration that a language option has been enabled
--   or disabled at the module level.
  | Feature Bool LanguageOption
-- | Declaration of a new datatype with a given name,
--   and a collection of constructors, which consist of a name, and
--   a list of argument types for those constructors.
  | DataType String [(String, [String])] deriving(Eq, Show, Lift, Generic)

instance Serialize SchemaEntry

-- Note: We should probably refactor these all into newtypes, incase we
-- want to make use of smart construtors later.
type TypeDecl   = String
type EntityDecl = (String, String)
type RelDecl    = (String, [String])
type DataTypeDecl = (String, [(String, [String])])
type Schema = [SchemaEntry]

-- Note: This might be useful still
-- | Helper function for converting from the new format for schemas to the old format.
-- getArities :: [TypedSchemaEntry] -> [SchemaEntry]
-- getArities [] = []
-- getArities ((Pred _ x ts _):xs)  = (x, length ts):(getArities xs)
-- getArities ((Type x):xs)     = getArities xs
-- getArities ((TypeOf _ _):xs) = getArities xs