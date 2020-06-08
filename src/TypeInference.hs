module TypeInference (
  inferType,
  insertTypeInformation
) where

-- Syntax & Types
import Syntax
import Types
import Examples

-- Type Inference
import ConstraintGeneration
import ConstraintUnification

-- Imports
import Control.Monad.State
import Control.Monad.Except
import Data.Either
import Data.List

-- infer the type of the expression
inferType :: Expression -> Either String Type
inferType expr = do
  result <- typeInference expr
  return $ fst result

-- insert type information to each term in the expression
insertTypeInformation :: Expression -> Either String Expression
insertTypeInformation expr = do
  result <- typeInference expr
  return $ snd result

-- type inference procedure
typeInference :: Expression -> Either String (Type, Expression)
typeInference expr = do

  -- CONSTRAINT GENERATION
  -- generate constraints along with type of expressions and constraints on annotations
  let cg = runState (generateConstraints [] expr) 1
  let ((context, expr_typed, typ, constraints, annoConstraints), counter) = cg

  -- CONSTRAINT UNIFICATION
  -- unify constraints and return gradual types and substitutions
  let cu = runState (unifyConstraints [([], constraints, [])]) counter
  let (gtypes', _, solutions') = unzip3 $ fst cu

  -- CHECK FOR TYPE ERRORS
  -- if there is no solution, then expression cannot be typed
  if null solutions' then throwError "Expression cannot be typed" else do

  -- ADD UNCONSTRAINED GRADUAL TYPE VARIABLES TO SUBSTITUTIONS
  -- obtain type variables from gradual types
  let gtypes = map (nub . concatMap (map VarType . collectTypeVariables)) gtypes'
  -- add substitutions from unconstrained type variables from gradual types to dynamic type
  let solutions = map (\x -> snd x ++ map (\x -> (x, DynType)) (fst x)) $ zip gtypes solutions'

  -- VERIFY IF ALL TYPE ANNOTATIONS ARE USED
  -- substitute annotations according to solutions
  let substitutedAnnoConstraints = map (\s -> map (\c -> foldr substituteConstraint c s) (filter (isIntersectionType . getFirstConstraint) annoConstraints)) solutions
  -- check for each annotation in each solution, if types obtained are equal to the corresponding type annotation modulo associativity, commutativity and idempotence
  let conformity = map (all (\x -> sameType (getFirstConstraint x) (getSecondConstraint x))) substitutedAnnoConstraints
  -- if no solution conforms, then expression cannot be typed
  if all (== False) conformity then throwError "Expression cannot be typed" else do

  -- CHOOSE SOLUTION WITH LESS DYNAMIC TYPES
  -- get number of dynamic type annotation choices 
  let dynamic_number = map (sum . map (length . filter isDynType . getInstancesType . getSecondConstraint)) substitutedAnnoConstraints
  -- obtain correct solutions
  let correct_solutions = map snd $ filter ((True ==) . fst) $ zip conformity $ zip dynamic_number solutions
  -- min number of dynamic annotations
  let min_dyn = minimum $ map fst correct_solutions
  -- obtain the most static solution
  let final_solution = head $ map snd $ filter ((min_dyn ==) . fst) correct_solutions
  -- discover final type by applying all substitutions to expression type t
  let finalType = removeRepeatedInstances $ foldr substituteType typ final_solution
  -- discover final types by applying all substitutions to each type annotation and type information in the expression
  let typedExpr = substituteTypedExpression final_solution expr_typed
  return (finalType, typedExpr)
