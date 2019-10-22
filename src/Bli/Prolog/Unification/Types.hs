
--
-- | Instance for unifying types.
--

module Bli.Prolog.Unification.Types where

import Data.Bli.Prolog.Ast
import Bli.Prolog.Unification
import Data.Bli.Prolog.Types

subsType :: (Unifier BliPrologType) -> BliPrologType -> BliPrologType
subsType u t@(TypeVar x) = maybe t id (lookup x u)
subsType u EntityT = EntityT
subsType u (FuncT dir x y) = FuncT dir (subsType u x) (subsType u y)
subsType u (DeclaredTypeT t) = DeclaredTypeT t
subsType u TypTypesT = TypTypesT
subsType u (GoalT ts) = GoalT (map (subsType u) ts)
subsType u (PredicateT ts) = PredicateT (map (subsType u) ts)
subsType u RuleT = RuleT
subsType u StringLitT = StringLitT
subsType u IntLitT = IntLitT
subsType u DateTimeLitT = DateTimeLitT
subsType u (ListT t) = ListT (subsType u t)
subsType u DateLitT = DateLitT

-- | Helper function to check if a variable occurs in a type.
occursIn :: Variable -> BliPrologType -> Bool
occursIn v (TypeVar x)  = v == x
occursIn v (GoalT ts) = any (occursIn v) ts
occursIn v (PredicateT ts) = any (occursIn v) ts
occursIn v (ListT t)   = occursIn v t
occursIn v (FuncT _ x y) = occursIn v x || occursIn v y
occursIn v _ = False

-- | Tries to unify two types.
unifyTypes :: BliPrologType -> BliPrologType -> Maybe (Unifier BliPrologType)
-- "_" matches anything
unifyTypes (TypeVar "_") t                                  = return []
unifyTypes t (TypeVar "_")                                  = return []
unifyTypes (TypeVar x) (TypeVar y) | x == y                 = return []
unifyTypes (TypeVar x) t | not (x `occursIn` t)             = return [(x, t)]
unifyTypes t v@(TypeVar _)                                  = unifyTypes v t
unifyTypes EntityT EntityT = return []
unifyTypes (FuncT dir x y) (FuncT dir' x' y') | dir == dir' = do
  unifiedXs <- unifyTypes x x' 
  unifiedYs <- unifyTypes y y'
  return $ unifiedXs ++ unifiedYs
unifyTypes (DeclaredTypeT t) (DeclaredTypeT t') | t == t'   = return [] 
unifyTypes TypTypesT TypTypesT                              = return []
unifyTypes (GoalT ts) (GoalT ts')                           = unifyListTypes ts ts'
unifyTypes (PredicateT ts) (PredicateT ts')                 = unifyListTypes ts ts'
unifyTypes RuleT RuleT                                      = return []
unifyTypes StringLitT StringLitT                            = return []
unifyTypes IntLitT IntLitT                                  = return []
unifyTypes DateTimeLitT DateTimeLitT                        = return []
unifyTypes (ListT t) (ListT t')                             = unifyTypes t t'
unifyTypes DateLitT DateLitT                                = return []
unifyTypes _ _                                              = Nothing

-- | Helper function which tries to unify a list of 
--   types simultaneously.
unifyListTypes :: [BliPrologType] -> [BliPrologType] -> Maybe (Unifier BliPrologType)
unifyListTypes (t : ts) (r : rs) =
    do u1 <- unifyTypes t r
       u2 <- unifyListTypes (map (subsType u1) ts) (map (subsType u1) rs)
       return $ u1 `compose` u2
unifyListTypes [] [] = Just []
unifyListTypes _ _   = Nothing

instance Unifiable BliPrologType where
  variables = typeVars
  subs      = subsType
  unify     = unifyTypes  
  

