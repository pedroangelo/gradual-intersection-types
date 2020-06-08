module ConstraintGeneration (
  generateConstraints
) where

-- Syntax & Types
import Syntax
import Types

-- Imports
import Control.Monad.State
import Data.Maybe
import Data.List

-- generate constraint set, type and context given an expression
generateConstraints :: Context -> Expression -> State Int (Context, Expression, Type, Constraints, Constraints)

-- (Cx) if expression is a variable
generateConstraints annoContext (Variable var typ) = do
  -- counter for variable creation
  i <- get
  put (i+1)
  -- create new type variable
  let t = newTypeVar i
  -- get context
  let context = [(var, t)]
  -- build typed expression
  let typedExpr = TypeInformation t $ Variable var t
  -- get type annotation intended for this Variable
  let annotation = lookup var annoContext
  if isNothing annotation then
    return (context, typedExpr, t, [], [])
  else do
    let (Just typeAnnotation) = annotation
    return (context, typedExpr, t, [Choice t typeAnnotation], [])

-- (Cabs:) if expression is an annotated abstraction
generateConstraints annoContext (AnnotatedAbstraction var typ expr) = do
  -- obtain principal pair from sub expression
  (context, typedExpr, t1, constraints, annoConstraints) <- generateConstraints ((var, typ) : annoContext) expr
  -- get context entry of bound variable
  let typeVar = lookup var context
  -- if there is no entry from bound variable
  if isNothing typeVar then do
    -- create new type
    let typ' = ArrowType typ t1
    -- build typed expression
    let typedExpr' = TypeInformation typ' $ AnnotatedAbstraction var typ typedExpr
    return (context, typedExpr', typ', constraints, annoConstraints)
  else do
    -- obtain type from bound variable
    let (Just typeOfBoundVariable) = typeVar
    -- create new type
    let typ' = ArrowType typeOfBoundVariable t1
    -- remove entry from context
    let context' = filter ((var /=) . fst) context
    -- build typed expression
    let typedExpr'= TypeInformation typ' $ AnnotatedAbstraction var typ typedExpr
    -- add annotation constraints
    let annoConstraints' = annoConstraints ++ removeSimpleConstraints [Equality typ typeOfBoundVariable]
    return (context', typedExpr', typ', constraints, annoConstraints')

-- (Capp) if expression is an application
generateConstraints annoContext (Application e1 e2) = do
  -- obtain principal pair from expression on the left
  (context1, typedExpr1, t1, constraints1, annoConstraints1) <- generateConstraints annoContext e1
  let
    result
      -- if domain type is an intersection
      | isArrowType t1 && isIntersectionType t11 = do
        let (IntersectionType t11s) = t11
        -- obtain diferent types according to the number of types in the intersection
        principalPairs <- replicateM (length t11s) $ generateConstraints annoContext e2
        let (contexts, typedExprs, types, constraints, annoConstraints) = unzip5 principalPairs
        -- add inequality constraints for each instance of the intersection type
        let constraints' = map (\x -> Inequality (fst x) (snd x)) $ zip types t11s
        -- join contexts
        let context' = foldl joinContext context1 contexts
        -- build typed expression
        let typedExpr = TypeInformation t12 $ Application typedExpr1 (foldr1 joinExpression typedExprs)
        -- add annotation constraints
        let annoConstraints' = annoConstraints1 ++ joinAnnoConstraints (concat annoConstraints)
        return (context', typedExpr, t12, constraints1 ++ concat (concat $ transpose [constraints, map (: []) constraints']), annoConstraints')
      | otherwise = do
        -- obtain principal pair from expression on the right
        (context2, typedExpr2, t2, constraints2, annoConstraints2) <- generateConstraints annoContext e2
        -- counter for variable creation
        i <- get
        put (i+2)
        -- create new type variable
        let t3 = newTypeVar i
        let t4 = newTypeVar (i+1)
        -- add constraints
        let constraints = constraints1 ++ [Equality t1 (ArrowType t3 t4)] ++ constraints2 ++ [Inequality t2 t3]
        -- join contexts
        let contexts = joinContext context1 context2
        -- build typed expression
        let typedExpr = TypeInformation t4 $ Application typedExpr1 typedExpr2
        return (contexts, typedExpr, t4, constraints, annoConstraints1 ++ annoConstraints2)
      where (ArrowType t11 t12) = t1
  result

-- (Cabs) if expression is an abstraction
generateConstraints annoContext (Abstraction var expr) = do
  -- obtain principal pair from sub expression
  (context, typedExpr', t1, constraints, annoConstraints) <- generateConstraints annoContext expr
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
    let typedExpr = TypeInformation typ' $ Abstraction var typedExpr'
    return (context, typedExpr, typ', constraints, annoConstraints)
  else do
    -- obtain type from bound variable
    let (Just typeOfBoundVariable) = typeVar
    -- create new type
    let typ' = ArrowType typeOfBoundVariable t1
    -- remove entry from context
    let context' = filter ((var /=) . fst) context
    -- build typed expression
    let typedExpr = TypeInformation typ' $ Abstraction var typedExpr'
    return (context', typedExpr, typ', constraints, annoConstraints)

-- (Cn) if expression is an integer
generateConstraints _ (Int n) = do
  let context = []
  let typedExpr = TypeInformation IntType $ Int n
  let typ = IntType
  let constraints = []
  let annoConstraints = []
  return (context, typedExpr, typ, constraints, annoConstraints)

-- (Cb) if expression is a boolean
generateConstraints _ (Bool b) = do
  let context = []
  let typedExpr = TypeInformation BoolType $ Bool b
  let typ = BoolType
  let constraints = []
  let annoConstraints = []
  return (context, typedExpr, typ, constraints, annoConstraints)

-- (C+) if expression is an addition
generateConstraints annoContext (Addition e1 e2) = do
  -- obtain principal pairs from sub expressions
  (context1, typedExpr1, t1, constraints1, annoConstraints1) <- generateConstraints annoContext e1
  (context2, typedExpr2, t2, constraints2, annoConstraints2) <- generateConstraints annoContext e2
  let typedExpression = TypeInformation IntType $ Addition typedExpr1 typedExpr2
  let typ = IntType
  let constraints = constraints1 ++ constraints2 ++ [Inequality IntType t1, Inequality IntType t2]
  return (joinContext context1 context2, typedExpression, typ, constraints, annoConstraints1 ++ annoConstraints2)

-- joins the bindings of two contexts
joinContext :: Context -> Context -> Context
joinContext [] c2 = c2
joinContext ((var, typ) : c1) c2 =
  let
    -- filter bindings in c2 with variable var
    bindings = filter ((var ==) . fst) c2
    -- context without the filtered bindings above
    c2' = filter ((var /=) . fst) c2
    -- create new binding for variable var by joining the types in both contexts
  in (var, foldl1 joinTypes $ typ : map snd bindings) : joinContext c1 c2'
{-    newBinding
      -- in case no binding of variable var in c2, bindings remains unaltered
      | bindings == [] = (var, typ)
      -- in case other bindings of variable var in c2, create new binding whose type is an intersection type of all types
      | otherwise = (var, IntersectionType $ (removeIntersectionType typ) ++ (concat $ map (removeIntersectionType . snd) bindings))
      where
        removeIntersectionType :: Type -> [Type]
        removeIntersectionType (IntersectionType ts) = ts
        removeIntersectionType t = [t]
  in newBinding : joinContext c1 c2'
-}

-- join same expression with different type annotations into one 
joinExpression :: Expression -> Expression -> Expression
joinExpression (Variable var1 typ1) (Variable var2 typ2) = Variable var1 $ joinTypes typ1 typ2
joinExpression (AnnotatedAbstraction var1 typ1 expr1) (AnnotatedAbstraction var2 typ2 expr2) = AnnotatedAbstraction var1 (joinTypes typ1 typ2) $ joinExpression expr1 expr2
joinExpression (Application e11 e12) (Application e21 e22) = Application (joinExpression e11 e21) (joinExpression e12 e22)
joinExpression (Abstraction var1 expr1) (Abstraction var2 expr2) = Abstraction var1 $ joinExpression expr1 expr2
joinExpression (TypeInformation typ1 expr1) (TypeInformation typ2 expr2) = TypeInformation (joinTypes typ1 typ2) $ joinExpression expr1 expr2
joinExpression (Addition e11 e12) (Addition e21 e22) = Addition (joinExpression e11 e21) (joinExpression e12 e22)
joinExpression (Int e1) (Int e2) = Int e1
joinExpression (Bool e1) (Bool e2) = Bool e1

-- join types in intersection types
joinTypes :: Type -> Type -> Type
joinTypes t1 t2
  -- if types are the same, return one
  | t1 == t2 = t1
  -- if both types are not intersection types, join them as instances of an intersection type
  | all (not . isIntersectionType) [t1,t2] = IntersectionType [t1,t2]
  -- join instances from both types in an intersection type
  | isIntersectionType t1 && not (isIntersectionType t2) = removeIntersectionType $ IntersectionType $ nub $ t1s ++ [t2]
  | isIntersectionType t2 && not (isIntersectionType t1) = removeIntersectionType $ IntersectionType $ nub $ t1 : t2s
  | otherwise = removeIntersectionType $ IntersectionType $ nub $ t1s ++ t2s
  where
    (IntersectionType t1s) = t1
    (IntersectionType t2s) = t2
    removeIntersectionType (IntersectionType ts)
      | length ts == 1 = head ts
      | otherwise = IntersectionType ts

-- remove non intersection type annotation constraints
removeSimpleConstraints :: Constraints -> Constraints
removeSimpleConstraints = filter (isIntersectionType . getFirstConstraint)

-- join constraints
joinAnnoConstraints :: Constraints -> Constraints
joinAnnoConstraints [] = []
joinAnnoConstraints (c:cs)
  -- constraints already joined
  | null sameAnnotation = c : joinAnnoConstraints cs
  -- join constraints with same intersection type
  | otherwise = let
    -- separate constraints relative to a different annotation
    rest = filter (\x -> t1 /= getFirstConstraint x) cs
    -- join types in intersection type
    intersectionType = foldl joinTypes t2 $ map getSecondConstraint sameAnnotation
    in Equality t1 intersectionType : joinAnnoConstraints rest
  where (Equality t1 t2) = c
        -- filter constraints relative to same annotation
        sameAnnotation = filter (\x -> t1 == getFirstConstraint x) cs
