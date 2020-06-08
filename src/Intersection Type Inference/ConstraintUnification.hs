module ConstraintUnification (
    unifyConstraints
) where

-- Syntax & Types
import Syntax
import Types

-- Imports
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

-- unify constraints to generate substitutions
unifyConstraints :: Constraints -> StateT Int (Except String) TypeSubstitutions
unifyConstraints [] = return []

-- for inequality constraints
unifyConstraints (Inequality t1 t2 : cs)
  -- U ((t11 -> t12 <=C t2) : cs)
  -- => U ((t3 <=C t11) : (t12 <=C t4) : (t2 =C t3 -> t4) : cs)
  | isArrowType t1 && isVarType t2 = do
    let (ArrowType t11 t12) = t1
    -- counter for variable creation
    i <- get
    put (i+2)
    -- create new type variables
    let t3 = newTypeVar i
    let t4 = newTypeVar (i+1)
    let constraints = [Inequality t3 t11, Inequality t12 t4, Equality t2 (ArrowType t3 t4)]
    unifyConstraints (constraints ++ cs)
  -- U ((t11 -> t12 <=C t21 -> t22) : cs)
  -- => U ((t21 <=C t11) : (t12 <=C t22) : cs)
  | isArrowType t1 && isArrowType t2 = do
    let (ArrowType t11 t12) = t1
    let (ArrowType t21 t22) = t2
    let constraints = [Inequality t21 t11, Inequality t12 t22]
    unifyConstraints (constraints ++ cs)
  -- U ((t1 <=C t21 ∩ ... ∩ t2n) : cs)
  -- => U ((t1 <=C t21) : ... : (t1 <=C t2n) : cs)
  | isIntersectionType t2 = do
    let (IntersectionType t2s) = t2
    let constraints = map (Inequality t1) t2s
    unifyConstraints (constraints ++ cs)
  -- U ((t1 <=C t2) : cs)
  -- => U ((t1 =C t2) : cs)
  | isVarType t1 && (not $ isIntersectionType t2) = do
    let constraints = [Equality t1 t2]
    unifyConstraints (constraints ++ cs)

-- for equality constraints
unifyConstraints (Equality t1 t2 : cs)
  -- U ((t =C t) : cs) => U cs
  | t1 == t2 = unifyConstraints cs
  -- U ((t11 -> t12 =C t21 -> t22) : cs)
  -- => U ((t12 =C t22) : (t11 =C t21) : cs)
  | isArrowType t1 && isArrowType t2 = do
    let (ArrowType t11 t12) = t1
    let (ArrowType t21 t22) = t2
    let constraints = [Equality t12 t22, Equality t11 t21]
    unifyConstraints (constraints ++ cs)
  -- U ((t1 =C t2) : cs), t1 ∉ TVars
  -- => U ((t2 =C t1) : cs)
  | not (isVarType t1) && isVarType t2 = do
    let constraints = [Equality t2 t1]
    unifyConstraints (constraints ++ cs)
  -- U ((t1 =C t2) : cs), t1 ∈ TVars
  -- => U ([t1 ↦ t2] cs) ∘ [t1 ↦ t2]
  | isVarType t1 && not (belongs t1 t2) = do
    let s = (t1, t2)
    substitutions <- unifyConstraints (map (substituteConstraint s) cs)
    return $ substitutions ++ [s]
  -- if no constraint matches, then throw error
  | otherwise = throwError $
    "Error: Types " ++ show t1 ++ " and " ++ show t2 ++ " are not equal!!"

-- test if type variable exists in typ
belongs :: Type -> Type -> Bool
belongs (VarType var) typ
  | isVarType typ =
    let (VarType var') = typ
    in var == var'
  | isArrowType typ =
    let (ArrowType t1 t2) = typ
    in belongs (VarType var) t1 || belongs (VarType var) t2
  | isIntersectionType typ =
    let (IntersectionType ts) = typ
    in any (belongs (VarType var)) ts
  | otherwise = False
belongs _ _ = False
