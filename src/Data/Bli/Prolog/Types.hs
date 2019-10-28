
module Data.Bli.Prolog.Types where

import Language.Haskell.TH.Lift
import Data.List
import Control.Monad
import Data.Serialize
import GHC.Generics

-- | Experimental, for lambek calculus support.
data Direction = LeftArr 
               | RightArr deriving(Eq, Show, Ord, Lift, Generic)

instance Serialize Direction

data BliPrologType where
   TypeVar :: String -> BliPrologType
   EntityT :: BliPrologType
 -- Function from one BliPrologType to another.
   FuncT :: Direction -> BliPrologType -> BliPrologType -> BliPrologType
 -- A user declared type, such as "person".
   DeclaredTypeT :: String -> BliPrologType
 -- Type of types, "type".
   TypTypesT :: BliPrologType
-- Note: \X. p(X), and p(X), as well as {p(X), q(Y)} are goals.
-- p is a predicate. If p is a binary predicate, then p(test) is a unary predicate.
   GoalT :: [BliPrologType] -> BliPrologType
   PredicateT :: [BliPrologType] -> BliPrologType
 -- A type for rules: This allows for an interesting
 -- design choice, where we allow for "first-class rules.",
 -- and so predicates are allowed to talk about rules.
   RuleT :: BliPrologType
   StringLitT :: BliPrologType
   IntLitT :: BliPrologType
   FloatLitT :: BliPrologType
   DateTimeLitT :: BliPrologType
 -- A polymorphic list datatype
   ListT :: BliPrologType -> BliPrologType
   DateLitT :: BliPrologType

-- | Helper function to get all of the type variables in
--   a BliPrologType.
typeVars :: BliPrologType -> [String]
typeVars (TypeVar v) = [v]
typeVars (ListT t) = typeVars t
typeVars (PredicateT ts) = join $ map typeVars ts
typeVars (GoalT ts) = join $ map typeVars ts
typeVars (FuncT _ x y) = typeVars x ++ typeVars y
typeVars _ = []

deriving instance Eq      BliPrologType
deriving instance Ord     BliPrologType
deriving instance Generic BliPrologType

instance Serialize BliPrologType

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
  show FloatLitT = "float"
  show DateTimeLitT = "datetime"
  show (ListT t) = "list["++ show t ++"]" 
  show DateLitT = "date"
