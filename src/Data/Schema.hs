
--
-- | Basic data types for dealing with blipl schemas, may also put 
--   some utility functions in this module for dealing with schemas
--   if needed.
--

module Data.Schema where

-- | A schema (curretly) is a list of identifiers,
--   together with their arity. In the future,
--   this will be a more complex datatype.
type Schema = [(String,Int)]