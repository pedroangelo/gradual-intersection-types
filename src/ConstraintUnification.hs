module ConstraintUnification (
  unifyConstraints
) where

-- Syntax & Types
import Syntax
import Types

-- Imports
import Control.Monad.State
import Data.Maybe

-- unify constraints to generate substitutions
unifyConstraints :: [([Type], Constraints, TypeSubstitutions)] -> State Int [([Type], Constraints, TypeSubstitutions)]
unifyConstraints solutions
  -- if all constraints in all solutions are solved, return result
  | all (null . snd3) solutions = return solutions
  -- if all constraints from first solution in the queue has been solved
  | (null . snd3 . head) solutions =
    -- pass solution to the last position
    unifyConstraints $ tail solutions ++ [head solutions]

-- unify inequality constraints
unifyConstraints ((types, Inequality t1 t2 : cs, subs) : solutions)
  | t1 == t2 =
    unifyConstraints ((types, cs, subs) : solutions)
  | isDynType t2 =
    unifyConstraints ((types ++ [t1], cs, subs) : solutions)
  | isDynType t1 =
    unifyConstraints ((types ++ [t2], cs, subs) : solutions)
  | isArrowType t1 && isArrowType t2 = do
    let (ArrowType t11 t12) = t1
    let (ArrowType t21 t22) = t2
    let constraints = [Inequality t21 t11, Inequality t12 t22]
    unifyConstraints ((types, constraints ++ cs, subs) : solutions)
  | isIntersectionType t2 = do
    let (IntersectionType t2s) = t2
    let constraints = map (Inequality t1) t2s
    unifyConstraints ((types, constraints ++ cs, subs) : solutions)
  | isArrowType t1 = do
    let (ArrowType t11 t12) = t1
    -- counter for variable creation
    i <- get
    put (i+2)
    -- create new type variables
    let t3 = newTypeVar i
    let t4 = newTypeVar (i+1)
    let constraints = [Inequality t3 t11, Inequality t12 t4, Equality t2 (ArrowType t3 t4)]
    unifyConstraints ((types, constraints ++ cs, subs) : solutions)
  | otherwise =
    unifyConstraints ((types, Equality t1 t2 : cs, subs) : solutions)

-- unify equality constraints
unifyConstraints ((types, Equality t1 t2 : cs, subs) : solutions)
  | t1 == t2 =
    unifyConstraints ((types, cs, subs) : solutions)
  | isIntersectionType t2 = do
    let (IntersectionType t2s) = t2
    let newSolutions = map (\x -> (types, Equality t1 x : cs, subs)) t2s
    unifyConstraints $ newSolutions ++ solutions
  | isArrowType t1 && isArrowType t2 = do
    let (ArrowType t11 t12) = t1
    let (ArrowType t21 t22) = t2
    let constraints = [Equality t11 t21, Equality t12 t22]
    unifyConstraints ((types, constraints ++ cs, subs) : solutions)
  | isDynType t1 && isArrowType t2 = do
    let (ArrowType t21 t22) = t2
    let constraints = [Equality t1 t21, Equality t1 t22]
    unifyConstraints ((types, constraints ++ cs, subs) : solutions)
  | isArrowType t1 && isDynType t2 = do
    let (ArrowType t11 t12) = t1
    let constraints = [Equality t11 t2, Equality t12 t2]
    unifyConstraints ((types, constraints ++ cs, subs) : solutions)
  | not (isVarType t1) && isVarType t2 = do
    let constraints = [Equality t2 t1]
    unifyConstraints ((types, constraints ++ cs, subs) : solutions)
  | isVarType t1 && not (belongs t1 t2) = do
    let s = (t1, t2)
    let constraints = map (substituteConstraint s) cs
    let types' = map (substituteType s) types
    unifyConstraints ((types', constraints, subs ++ [s]) : solutions)
  | otherwise =
    unifyConstraints solutions

-- unify choice constraints
unifyConstraints ((types, Choice t1 t2 : cs, subs) : solutions)
  | isIntersectionType t2 = do
    let (IntersectionType t2s) = t2
    let newSolutions = map (\x -> (types, Equality t1 x : cs, subs)) t2s
    unifyConstraints $ newSolutions ++ solutions
  | otherwise =
    unifyConstraints ((types, Equality t1 t2 : cs, subs) : solutions)

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
