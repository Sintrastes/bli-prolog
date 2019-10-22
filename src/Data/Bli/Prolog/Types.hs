
module Data.Bli.Prolog.Types where
data Polymorphic
data Monomorphic

-- | A Bli Prolog type can either be polymorphic (contains type variables),
--   or monomorphic (does not contain type variables).
--   The instances of the typeclass TypeOfBliType allow us to discriminate between
--   these two types of Bli Prolog types.
class TypeOfBliType where
  instance TypeOfBliType Polymorphic
  instance TypeOfBliType Monomorphic

-- | CombineMode is a type family which is used to define how
--   combining two different types of BliProlog types works.
--   
--   If we combine a polymorphic type with anything, we get
--   another polymorphic type.
--
--   If we combine two monomorphic types, we get a
--   monomorphic type.
--
type family CombineMode a b
  type instance CombineMode Polymorphic Polymorphic = Polymorphic
  type instance CombineMode Polymorphic Monomorphic = Polymorphic
  type instance CombineMode Monomorphic Polymorphic = Polymorphic
  type instance CombineMode Monomorphic Monomorphic = Monomorphic

data BliPrologType where
   TypeVar :: String -> (BliPrologType Monomorphic) -> BliPrologType Polymorphic
   EntityT :: BliPrologType Monomorphic
 -- Function from one BliPrologType to another.
   FuncT :: BliPrologType a -> BliPrologType b -> BliPrologTypes (CombineMode a b)
 -- A user declared type, such as "person".
   DeclaredTypeT :: String -> BliPrologType Monomorphic
 -- Type of types, "type".
   TypTypesT :: String -> BliPrologType Monomorphic
-- Note: \X. p(X), and p(X), as well as {p(X), q(Y)} are goals.
-- p is a predicate. If p is a binary predicate, then p(test) is a unary predicate.
   GoalT :: [BliPrologType a] -> BliPrologType a
   PredicateT :: [BliPrologType a] -> BliPrologType a
 -- A type for rules: This allows for an interesting
 -- design choice, where we allow for "first-class rules.",
 -- and so predicates are allowed to talk about rules.
   RuleT :: BliPrologType Monomorphic
   StringLitT :: BliPrologType Monomorphic
   IntLitT :: BliPrologType Monomprphic
   DateTimeLitT :: BliPrologType Monomorphic
 -- A polymorphic list datatype
   ListT :: BliPrologType a -> BliPrologType a
   DateLitT :: BliPrologType Monomorphic

instance Eq  BliPrologType where
instance Ord BliPrologType where

instance Show BliPrologType where
  show EntityT = "entity"
  show (DeclaredTypeT str) = str
  show TypTypesT = "type"
  show (PredicateT []) = "pred"
  show (PredicateT types) = "pred[" ++ (intercalate ", " (map show types)) ++ "]"
  show (GoalT []) = "goal"
  show (GoalT types) = "goal[" ++ (intercalate ", " (map show types)) ++ "]"
  show RuleT = "rule"
  show StringLitT = "string"
  show IntLitT = "int"
  show DateTimeLitT = "datetime"
  show (ListT t) = "list["++ show t ++"]" 
  show DateLitT = "date"
