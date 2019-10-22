
--
-- | General utility functions used by bedelibry prolog.
--

module Bli.Util where

import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Schema

-- | Get all of the relevant data from a schema file and group it
--   into the appropriate lists
groupSchemaClauses :: BliProgram -> ([TypeDecl], [RelDecl], [EntityDecl], [Clause])
groupSchemaClauses commands = go commands ([], [], [], []) 
 where go [] xs = xs
       go ((AssertClause c):xs) (types,relations,entities,clauses)
           = go xs (types, relations, entities, c:clauses) -- Not relevant for a schema file.
       go ((AssertSchema (Type t)):xs) (types,relations,entities,clauses)
           = go xs (t:types, relations, entities, clauses)
       go ((AssertSchema (TypeOf t ty)):xs) (types,relations,entities,clauses)
           = go xs (types, relations, (t,ty):entities, clauses)
       go ((AssertSchema (Pred isStored name argTypes dirs)):xs) (types,relations,entities,clauses)
           = go xs (types, (name, argTypes):relations, entities, clauses)
       -- Note: This will need to be changed to use the
       --       Bli monad, since processing this further requires looking up 
       --       other modules
       go ((AssertSchema (Using modName)):xs) (types, relations, entities, clauses) = undefined

-- | Get all of the relevant data from a bli prolog file, and group it into the relevant lists.
--   where the final component of the return type is a list of modules which have to be
--   imported
groupClauses :: BliProgram -> ([TypeDecl], [RelDecl], [EntityDecl], [Clause], [String])
groupClauses commands = go commands ([], [], [], [], []) 
 where go [] xs = xs
       go ((AssertClause c):xs) (types,relations,entities,clauses,modules)
           = go xs (types, relations, entities, c:clauses,modules)
       go ((AssertSchema (Type t)):xs) (types,relations,entities,clauses,modules)
           = go xs (t:types, relations, entities, clauses, modules)
       go ((AssertSchema (TypeOf t ty)):xs) (types,relations,entities,clauses,modules)
           = go xs (types, relations, (t,ty):entities, clauses, modules)
       go ((AssertSchema (Pred isStored name argTypes dirs)):xs) (types,relations,entities,clauses,modules)
           = go xs (types, (name, argTypes):relations, entities, clauses,modules)
       go ((AssertSchema (Using modName)):xs) (types, relations, entities, clauses, modules)
           = go xs (types, relations, entities, clauses, modName:modules)
