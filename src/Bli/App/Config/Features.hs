
module Bli.App.Config.Features where

import Language.Haskell.TH.Lift

-- | The default language options that bedelibry prolog
--   uses (without language pragmas, or specifications
--   to use other language features in the config file.
defaultLanguageOptions = 
   [
     TypePredicates
   , Aliases
   , DatatypeOverloading
   , EquationalSyntax
   , StoredRelations
   , Procedures
   , ExternalProcedures
   , PartialApplication
   , UnicodeSyntax
   ]

-- | Note: These are things which should not be able to be configured through
--   the command line. Or, if they are, it should be through a very specific interface
--   (like Haskell's -XWhateverExtension).
--
--   Most likely, these should be read from a configuration file, or from
--   the command line, or using language pragmas. 
--
--   This is mostly for me to experiment around with different things. I will
--   probably set some defaults eventually
data LanguageOption =
-- | This gives access to the untyped version of bli prolog, and the original syntax
-- for schema files, if anyone wants access to that fragment of the language.
    UntypedLanguage
-- | When set, this allows for
--   names to be used unambigously in different ways.
--
--   For instance, programming_langauge can refer to a type, as well as
--   a unary predicate on entities.
--
--   However, this extension allows user-declared predicates to have multiple
--   roles. For instance, a predicate "pred : type -> type -> type" under this
--   extension is considered to be different from a predicate "pred: type -> type".   
   |  PredicateOverloading
-- | This works similarly to the above, except it allows for entities
--   with the same name to be declared of different types.
--   This is a bit more confusing, because of the ambiguity in how this
--   should be interpreted semantically. Does 
--
--       nate : person
--       nate : animal
--
--   mean that nate (person) and nate (animal) refer to two different entities,
--   or does this mean that nate is both a person and an animal?
--
--   This makes entity overloading a more difficult extension to implement.
--   Perhaps this might be better a multiple extensions with slightly different
--   behavior. Otherwise, we need a system here for how these sort of ambiguities
--   will be handled.   
   | EntityOverloading
-- | Allows for predicates on entities which returns whether or not
--   a given entity is of that type.
   | TypePredicates
-- | Allows for user-declared aliases.
   | Aliases
-- Allows for user-declared subtyping
   | Subtyping
-- | This extension is pretty straightforward (I think). It allows for an identifier
--   to stand for both an entity, and for a predicate. For instance, programming_language
--   is an entity of type *type*, whereas programming_language is also a unary predicate.
--   With this extension set, users are allowed to declare their own identifiers
--   as being predicates and as being entities. This should be relatively straightforward
--   because there is no way of confusing the semantics here.
   | PredicateEntityOverloading
-- | This extension allows
   | DatatypeOverloading
-- | Optional syntatic sugar for defining body-less clauses.
   | EquationalSyntax
-- | Allows for the setting of relations with a "stored" attribute
--   that tells the bli prolog runtime that these relations
--   are stored in the bedelibry storage backend.
   | StoredRelations
-- | Allows for the declaration of relations that are not stored
--   in the bedelibry server backend, but otherwise require some
--   "real world" action to retrieve that data when queried.
   | ExternRelations
-- | Enables the use of procedures.
   | Procedures
-- | Enables the use of external procedures, which are declared in a 
--   Haskell module that the user specifies
   | ExternalProcedures
-- | Enables the use of external Haskell relations, which are similar to external relations,
--   but use a Haskell function of type `a -> [b]` to encode relations.
--   Note: There is a lot of overlap here with the ExtenRelations extension, 
--   and I might merge these in the future.
   | ExternalHsRelations
-- | Enables basic partial applications of predicates. If an argument is not supplied
--   at the beginning or the end of a predicate, then the user can partially apply.
--
--   Example:
--     rel p: person, int.
--     p(nate) ~> \X. p(nate, X)
--   
--   Note that with this option enabled,
--   partial application will only work left-to-right. 
--   In other words, the followin example will fail:
--
--   Example:
--     rel p: person, int.
--     p(24).
--        > Syntax Error. Bad partial application. 
--
--   Even though this example is fine with AdvancedPartialApplication enabled.
--
   | PartialApplication
-- | Enables advanced partial application of predicates. If a predicate
--   has a single missing field anywhere in the predicate, and the 
--   positions of its other arguments are unambigious, then
--   the predicate will be partially applied.
--
--   Example:
--     rel p: person, person, int. 
--     p(nate,24) ~> \X. p(nate,X,24).
--
--   Note that if a result is ambigous, such as the following, the behavior will default to
--   standard partial appliation (i.e. application of terms from left to right.
--
--  Example:
--    rel p: person, person.
--    p(nate) ~> \X. p(nate,X).
--  
--  However, this case can be configured either to be an error, or
--  to give a warning if the options WarnAmbigiousPartialApplication
--  or ErrorAmbigiousPartialApplication are enabled. 
--  
   | AdvancedPartialApplication
-- | If this option is enabled, the typechecker will give a warning
--   for examples where advanced partial application is ambigious, but
--   we can still meaningfully use the standard behavior of partial applications.
   | WarnAmbigiousPartialApplication
-- | If this option is enabled, the typechecker will give an error
--   for examples where advanced partial application is ambigious, but
--   we can still meaningfully use the standard behavior of partial applications.
   | ErrorAmibigiousPartialApplication
-- | Enables the use of unicode syntax instead of the default ascii, e.x. → and ← for arrows, Λ or λ for lambda expressions.
   | UnicodeSyntax
-- | Enables relations to be defined with the symbols -> and <- to denote on which side an argument is expected. This allows for 
--   applying predicates on the left and on the right, which allows for queries to be made in a natural language like syntax.
   | LambekTypes
-- | The following features are marked as experimental, and
--   should only be enabled with caution
  deriving(Eq, Show, Lift)
experimentalFeatures =
  [
    ExternalHsRelations
  , Procedures
  , AdvancedPartialApplication
  , LambekTypes
  ]