module ConstraintGeneration (
    generateConstraints
) where

-- Syntax & Types
import Syntax
import Types

-- Imports
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Data.List

-- generate constraint set, type and context given an expression
generateConstraints :: Expression -> StateT Int (Except String) (Type, Constraints, Context, Expression)

-- (Cx) if expression is a variable
generateConstraints (Variable var typ) = do
  -- counter for variable creation
  i <- get
  put (i+1)
  -- create new type variable
  let t = newTypeVar i
  -- get constraints
  let constraints = []
  -- get context
  let context = [(var, t)]
  -- build typed expression
  let typedExpr = TypeInformation t $ Variable var t
  return (t, constraints, context, typedExpr)

-- (Cλ) if expression is a abstraction
generateConstraints (Abstraction var typ expr) = do
  -- obtain principal pair from sub expression
  (t1, constraints, context, typedExpr') <- generateConstraints expr
  -- get context entry of bound variable
  let typeVar = lookup var context
  -- if there is no entry from bound variable
  if isNothing typeVar then do
    -- counter for variable creation
    i <- get
    put (i+1)
    -- create new type variable
    let t = newTypeVar i
    -- create new type
    let typ' = ArrowType t t1
    -- build typed expression
    let typedExpr = TypeInformation typ' $ Abstraction var t $ typedExpr' 
    return (typ', constraints, context, typedExpr)
  else do
    -- obtain type from bound variable
    let (Just typeOfBoundVariable) = typeVar
    -- create new type
    let typ' = ArrowType typeOfBoundVariable t1
    -- remove entry from context
    let context' = filter (\x -> (fst x) /= var) context
    -- build typed expression
    let typedExpr = TypeInformation typ' $ Abstraction var typeOfBoundVariable typedExpr'
    return (typ', constraints, context', typedExpr)

-- (Capp) if expression is an application
generateConstraints (Application e1 e2) = do
  -- obtain principal pair from expression on the left
  (t1, constraints1, context1, typedExpr1) <- generateConstraints e1
  let
    result
      -- if domain type is an intersection
      | isArrowType t1 && isIntersectionType t11 = do
          let (IntersectionType t11s) = t11
          -- obtain diferent types according to the number of types in the intersection
          principalPairs <- replicateM (length t11s) $ generateConstraints e2
          let (types, constraints, contexts, typedExprs) = unzip4 principalPairs
          -- add inequality constraints for each instance of the intersection type
          let constraints' = map (\x -> Inequality (fst x) (snd x)) $ zip types t11s
          -- join contexts
          let context' = foldl joinContext context1 contexts
          -- build typed expression
          let typedExpr = TypeInformation t12 $ Application typedExpr1 (foldr1 joinExpression typedExprs)
          return (t12, constraints1 ++ (concat constraints) ++ constraints', context', typedExpr)
      | otherwise = do
          -- obtain principal pair from expression on the right
          (t2, constraints2, context2, typedExpr2) <- generateConstraints e2
          -- counter for variable creation
          i <- get
          put (i+2)
          -- create new type variable
          let t3 = newTypeVar i
          let t4 = newTypeVar (i+1)
          -- add constraints
          let constraints = constraints1 ++ constraints2 ++ [Equality t1 (ArrowType t3 t4), Inequality t2 t3]
          -- join contexts
          let contexts = joinContext context1 context2
          -- build typed expression
          let typedExpr = TypeInformation t4 $ Application typedExpr1 typedExpr2
          return (t4, constraints, contexts, typedExpr)
      where (ArrowType t11 t12) = t1
  result

-- joins the bindings of two contexts
joinContext :: Context -> Context -> Context
joinContext [] c2 = c2
joinContext ((var, typ) : c1) c2 =
  let
    -- filter bindings in c2 with variable var
    bindings = filter (\x -> fst x == var) c2
    -- context without the filtered bindings above
    c2' = filter (\x -> fst x /= var) c2
    -- create new binding for variable var by joining the types in both contexts
    newBinding
      -- in case no binding of variable var in c2, bindings remains unaltered
      | bindings == [] = (var, typ)
      -- in case other bindings of variable var in c2, create new binding whose type is an intersection type of all types
      | otherwise = (var, IntersectionType $ (removeIntersectionType typ) ++ (concat $ map (removeIntersectionType . snd) bindings))
      where
        removeIntersectionType :: Type -> [Type]
        removeIntersectionType (IntersectionType ts) = ts
        removeIntersectionType t = [t]
  in newBinding : joinContext c1 c2'

-- join expression
joinExpression :: Expression -> Expression -> Expression
joinExpression (Variable var1 typ1) (Variable var2 typ2) = Variable var1 $ joinTypes typ1 typ2
joinExpression (Abstraction var1 typ1 expr1) (Abstraction var2 typ2 expr2) = Abstraction var1 (joinTypes typ1 typ2) $ joinExpression expr1 expr2
joinExpression (Application e11 e12) (Application e21 e22) = Application (joinExpression e11 e21) (joinExpression e12 e22)
joinExpression (TypeInformation typ1 expr1) (TypeInformation typ2 expr2) = TypeInformation (joinTypes typ1 typ2) $ joinExpression expr1 expr2

-- join types in intersection types
joinTypes :: Type -> Type -> Type
joinTypes t1 t2
  | t1 == t2 = t1
  | (not $ isIntersectionType t1) && (not $ isIntersectionType t2) = IntersectionType [t1,t2]
  | isIntersectionType t1 && (not $ isIntersectionType t2) = IntersectionType $ t1s ++ [t2]
  | isIntersectionType t2 && (not $ isIntersectionType t1) = IntersectionType $ t1 : t2s
  | otherwise = IntersectionType $ t1s ++ t2s
  where
    (IntersectionType t1s) = t1
    (IntersectionType t2s) = t2
    

{-

-- (Cx) if expression is a variable
generateConstraints (ctx, Variable var) = do
    -- obtain type from context
    let varType = lookup var ctx
    -- check if variable exists in context
    if isNothing varType then
        -- if not, throw error
        throwError $ "Error: Variable " ++ var ++ " does not exist!! Terms must be closed!!"
    else do
        -- retrieve type
        let (finalType, constraints) = fromJust varType
        i <- get
        -- replace quantified variables by type variables
        -- instantiation of Damas-Milner
        let ((finalType', constraints'), i') =
                runState (replaceQuantifiedVariables finalType constraints) i
        put i'
        -- build typed expression
        let typedExpr = TypeInformation finalType' (Variable var)
        -- return type
        return (finalType', constraints', typedExpr)

-- (Cλ) if expression is a abstraction
generateConstraints (ctx, Abstraction var expr) = do
    -- counter for variable creation
    i <- get
    put (i+1)
    -- create new type variable
    let newVar1 = newTypeVar i
    -- create a binding between the abstraction variable and the new type variable
    let binding = (var, (ForAll "" newVar1, []))
    -- build type assignment with new binding
    let typeAssignment = (binding : ctx, expr)
    -- obtain type and generate constraints for new type assignment
    (exprType, constraints, expr_typed) <- generateConstraints typeAssignment
    -- build typed expression
    let typedExpr = TypeInformation (ArrowType newVar1 exprType) (Abstraction var expr_typed)
    -- return arrow type and constraints
    return (ArrowType newVar1 exprType, constraints, typedExpr)

-- (Capp) if expression is a application
generateConstraints (ctx, Application expr1 expr2) = do
    -- build for each expression in the application
    -- a type assignment
    let typeAssignment1 = (ctx, expr1)
    let typeAssignment2 = (ctx, expr2)
    -- obtain type and constraints for both expressions
    (t1, constraints1, expr1_typed) <- generateConstraints typeAssignment1
    (t2, constraints2, expr2_typed) <- generateConstraints typeAssignment2
    -- get constraints for codomain and domain relation
    (t3, constraints3) <- codomain t1
    constraints4 <- domain t1 t2
    -- build typed expression
    let typedExpr = TypeInformation t3 (Application expr1_typed expr2_typed)
    -- return type along with all the constraints
    return (t3, constraints1 ++ constraints2 ++ constraints3 ++ constraints4, typedExpr)

-- (Cλ:) if expression is a annotated abstraction
generateConstraints (ctx, Annotation var typ expr) = do
    -- create a binding between the abstraction variable and the annotated type
    let binding = (var, (ForAll "" typ, []))
    -- build type assignment with new binding
    let typeAssignment = (binding : ctx, expr)
    -- obtain type and generate constraints for new type assignment
    (exprType, constraints, expr_typed) <- generateConstraints typeAssignment
    -- build typed expression
    let typedExpr = TypeInformation (ArrowType typ exprType) (Annotation var typ expr_typed)
    -- return arrow type and constraints
    return (ArrowType typ exprType, constraints, typedExpr)

-- (Cn) if expression is a integer
generateConstraints (ctx, Int int) = do
    -- build typed expression
    let typedExpr = TypeInformation IntType (Int int)
    -- return Int type
    return (IntType, [], typedExpr)

-- (Cb) if expression is a boolean
generateConstraints (ctx, Bool bool) = do
    -- build typed expression
    let typedExpr = TypeInformation BoolType (Bool bool)
    -- return Bool type
    return (BoolType, [], typedExpr)

-- if expression is an arithmetic or relational operator
generateConstraints (ctx, expr)
    -- if expression is an addition (C+), subtraction (C-),
    -- multiplication (C*), or division (C/)
    | isArithmeticOperator expr = do
        -- build for each expression in the addition a type assignment
        let typeAssignment1 = (ctx, expr1)
        let typeAssignment2 = (ctx, expr2)
        -- obtain type and constraints for both expressions
        (t1, constraints1, expr1_typed) <- generateConstraints typeAssignment1
        (t2, constraints2, expr2_typed) <- generateConstraints typeAssignment2
        -- build typed expression
        let expr_typed
                | isAddition expr = Addition expr1_typed expr2_typed
                | isSubtraction expr = Subtraction expr1_typed expr2_typed
                | isMultiplication expr = Multiplication expr1_typed expr2_typed
                | isDivision expr = Division expr1_typed expr2_typed
        -- insert type information
        let typedExpr = TypeInformation IntType expr_typed
        -- return type along with all the constraints
        return (IntType, constraints1 ++ constraints2 ++
            [Consistency t1 IntType, Consistency t2 IntType], typedExpr)
    -- if expression is an equality (C==), not equality (C/=), lesser than (C<),
    -- greater than (C>), lesser than or equal to (C<=) or greater than or equal to (C>=) check
    | isRelationalOperator expr = do
        -- build for each expression in the addition a type assignment
        let typeAssignment1 = (ctx, expr1)
        let typeAssignment2 = (ctx, expr2)
        -- obtain type and constraints for both expressions
        (t1, constraints1, expr1_typed) <- generateConstraints typeAssignment1
        (t2, constraints2, expr2_typed) <- generateConstraints typeAssignment2
        -- build typed expression
        let expr_typed
                | isEqual expr = Equal expr1_typed expr2_typed
                | isNotEqual expr = NotEqual expr1_typed expr2_typed
                | isLessThan expr = LesserThan expr1_typed expr2_typed
                | isGreaterThan expr = GreaterThan expr1_typed expr2_typed
                | isLessEqualTo expr = LesserEqualTo expr1_typed expr2_typed
                | isGreaterEqualTo expr = GreaterEqualTo expr1_typed expr2_typed
        -- insert type information
        let typedExpr = TypeInformation BoolType expr_typed
        -- return type along with all the constraints
        return (BoolType, constraints1 ++ constraints2 ++
            [Consistency t1 IntType, Consistency t2 IntType], typedExpr)
    -- retrieve sub expressions from the operator
    where (expr1, expr2) = fromOperator expr

-- (C:) if expression if a type information
generateConstraints (ctx, e@(TypeInformation typ expr)) = do
    -- build type assignment
    let typeAssignment = (ctx, expr)
    -- obtain type and generate constraints for type assignment
    (t, constraints, _) <- generateConstraints typeAssignment
    -- return type along with all the constraints
    return (t, constraints, e)
-}
