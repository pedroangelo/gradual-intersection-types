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
    -- generate constraints
    cg <- runExcept $ runStateT (generateConstraints 0 expr) 1
    -- retrieve constraints
    let ((typ, constraints, constraintsAnno, context, expr_typed), counter) = cg
    -- unify constraints and generate substitutions
    (substitutions, _) <- runExcept $ runStateT (unifyConstraints constraints) counter
    -- discover final type by applying all substitutions to expression type t
    let finalType = foldr substituteType typ substitutions
    -- discover final types by applying all substitutions to each type annotation and type information in the expression
    let typedExpr = substituteTypedExpression substitutions expr_typed
    return (finalType, typedExpr)
