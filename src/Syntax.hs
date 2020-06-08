module Syntax where

-- Types
import Types

-- Imports
import Data.List
import Control.Monad.State

-- Expressions in λ-calculus and extensions
data Expression
    -- λ-calculus terms
    = Variable Var Type
    | AnnotatedAbstraction Var Type Expression
    | Application Expression Expression
    | Abstraction Var Expression
    -- Integers
    | Int Int
    -- Booleans
    | Bool Bool
    -- Arithmetic operator
    | Addition Expression Expression
    -- Type annotations
    | TypeInformation Type Expression
    -- Cast Intersection
    | CastIntersection [Cast] Expression
    -- Blames
    | Blame Type Message
    deriving (Show, Eq)

-- Cast inhabiting intersection of casts
data Cast
    = SingleCast CastLabel Type Type Cast
    | BlameCast CastLabel Type Type Message
    | EmptyCast CastLabel Type
    deriving (Show, Eq)

type CastLabel = Int

-- MAPPING

-- Expression Mapping
mapExpression :: (Expression -> Expression) -> Expression -> Expression

-- λ-calculus terms
mapExpression f e@(Variable var typ) = f e
mapExpression f e@(AnnotatedAbstraction var typ expr) =
    f (AnnotatedAbstraction var typ $ mapExpression f expr)
mapExpression f e@(Abstraction var expr) =
    f (Abstraction var $ mapExpression f expr)
mapExpression f e@(Application expr1 expr2) =
    f (Application (mapExpression f expr1) (mapExpression f expr2))

-- Integers
mapExpression f e@(Int int) = f e

-- Booleans
mapExpression f e@(Bool bool) = f e

-- Arithmetic operators
mapExpression f e@(Addition expr1 expr2) =
    f (Addition (mapExpression f expr1) (mapExpression f expr2))

-- Type annotations
mapExpression f e@(TypeInformation typ expr) =
    f (TypeInformation typ (mapExpression f expr))

-- Cast Intersection
mapExpression f e@(CastIntersection cs expr) =
    f (CastIntersection cs (mapExpression f expr))

-- Blames
mapExpression f e@(Blame typ label) = f e

-- Expression Mapping
mapCast :: (Cast -> Cast) -> Cast -> Cast
mapCast f (SingleCast cl t1 t2 cs) =
    f (SingleCast cl t1 t2 $ mapCast f cs)
mapCast f c@BlameCast{} = f c
mapCast f c@EmptyCast{} = f c

-- CHECKS

-- check if it's a variable
isVariable :: Expression -> Bool
isVariable Variable{} = True
isVariable _ = False

-- check if it's an annotated abstraction
isAnnotatedAbstraction :: Expression -> Bool
isAnnotatedAbstraction AnnotatedAbstraction{} = True
isAnnotatedAbstraction _ = False

-- check if it's an application
isApplication :: Expression -> Bool
isApplication Application{} = True
isApplication _ = False

-- check if it's an abstraction
isAbstraction :: Expression -> Bool
isAbstraction Abstraction{} = True
isAbstraction _ = False

-- check if it's a lambda abstraction
isLambdaAbstraction :: Expression -> Bool
isLambdaAbstraction e = isAnnotatedAbstraction e || isAbstraction e 

-- check if it's a boolean
isBool :: Expression -> Bool
isBool Bool{} = True
isBool _ = False

-- check if it's an integer
isInt :: Expression -> Bool
isInt Int{} = True
isInt _ = False

-- check if it's an addition
isAddition :: Expression -> Bool
isAddition Addition{} = True
isAddition _ = False

-- check if it's a type information
isTypeInformation :: Expression -> Bool
isTypeInformation TypeInformation{} = True
isTypeInformation _ = False

-- check if it's a cast intersection
isCastIntersection :: Expression -> Bool
isCastIntersection CastIntersection{} = True
isCastIntersection _ = False

-- check if it's a blame
isBlame :: Expression -> Bool
isBlame Blame{} = True
isBlame _ = False

-- check if it's a single cast
isSingleCast :: Cast -> Bool
isSingleCast SingleCast{} = True
isSingleCast _ = False

-- check if it's a blame cast
isBlameCast :: Cast -> Bool
isBlameCast BlameCast{} = True
isBlameCast _ = False

-- check if it's an empty cast
isEmptyCast :: Cast -> Bool
isEmptyCast EmptyCast{} = True
isEmptyCast _ = False

-- check if it's a value
isValue :: Expression -> Bool
isValue e =
    isVariable e ||
    isAnnotatedAbstraction e ||
    isAbstraction e ||
    isBool e ||
    isInt e ||
    isValueCastIntersection e ||
    isBlame e

-- check if intersection of casts is a value
isValueCastIntersection :: Expression -> Bool
isValueCastIntersection (CastIntersection cs e) =
    all isCastValue cs &&
    not (all isEmptyCast cs) &&
    not (all isBlameCast cs) &&
    isValue e
isValueCastIntersection _ = False

-- check if casts in cast intersection is a value in the cast semantics
isCastValue :: Cast -> Bool
isCastValue c = isCastValue1 c || isCastValue2 c

-- check if casts in cast intersection is a value in the cast semantics
isCastValue1 :: Cast -> Bool
isCastValue1 (SingleCast _ t1 t2 c) =
    (isGroundType t1 && isDynType t2 && isCastValue1 c) ||
    (isArrowType t1 && isArrowType t2 && t1 /= t2 && isCastValue1 c) ||
    (isGroundType t1 && isDynType t2 && isEmptyCast c) ||
    (isArrowType t1 && isArrowType t2 && t1 /= t2 && isEmptyCast c)
isCastValue1 _ = False

-- check if casts in intersection of casts is a value in the cast semantics
isCastValue2 :: Cast -> Bool
isCastValue2 c = isBlameCast c || isEmptyCast c

-- PROJECTIONS

-- get variable and subexpression from abstractions
fromAbstraction :: Expression -> (String, Expression)
fromAbstraction (AnnotatedAbstraction var _ expr) = (var, expr)
fromAbstraction (Abstraction var expr) = (var, expr)

-- get type from type information
fromTypeInformation :: Expression -> Type
fromTypeInformation (TypeInformation typ _) = typ

-- get cast label
getCastLabel :: Cast -> CastLabel
getCastLabel (SingleCast cl _ _ _) = cl
getCastLabel (BlameCast cl _ _ _) = cl
getCastLabel (EmptyCast cl _) = cl

-- get initial type
initialType :: Cast -> Type
initialType (SingleCast cl t1 t2 c) = initialType c
initialType (BlameCast cl i f msg) = i
initialType (EmptyCast cl t) = t

-- get final type
finalType :: Cast -> Type
finalType (SingleCast cl t1 t2 c) = t2
finalType (BlameCast cl i f msg) = f
finalType (EmptyCast cl t) = t

-- project from 3 tuple
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

-- SUBSTITUTIONS
type ExpressionSubstitution = (String, Expression)

-- Substitute expressions according to substitution
substitute :: ExpressionSubstitution -> Expression -> Expression

-- λ-calculus terms
substitute s@(old, new) e@(Variable var typ)
    -- if var equals old, replace variable with new expression
    | var == old = instantiateCastIntersection typ new
    -- otherwise, do nothing
    | otherwise = e
substitute s@(old, new) e@(AnnotatedAbstraction var typ expr)
    -- if some abstraction has already binded the variable, don't propagate substitutions
    | var == old = e
    -- otherwise, propagate substitutions
    | otherwise = AnnotatedAbstraction var typ $ substitute s expr
substitute s@(old, new) e@(Application expr1 expr2) =
    Application (substitute s expr1) (substitute s expr2)
substitute s@(old, new) e@(Abstraction var expr)
    -- if some abstraction has already binded the variable, don't propagate substitutions
    | var == old = e
    -- otherwise, propagate substitutions
    | otherwise = Abstraction var $ substitute s expr

-- Integers
substitute s@(old, new) e@(Bool _) = e

-- Booleans
substitute s@(old, new) e@(Int _) = e

-- Arithmetic operators
substitute s@(old, new) e@(Addition expr1 expr2) =
    Addition (substitute s expr1) (substitute s expr2)

-- Type annotation
substitute s@(old, new) e@(TypeInformation typ expr) =
    TypeInformation typ $ substitute s expr

-- Cast Intersection
substitute s@(old, new) e@(CastIntersection cs expr) =
    CastIntersection cs $ substitute s expr

-- Blames
substitute s@(old, new) e@(Blame t1 label) = e

-- instantiate cast intersection according to expected type of variable
instantiateCastIntersection :: Type -> Expression -> Expression
instantiateCastIntersection typ expr
  | not $ isCastIntersection expr = expr
  | otherwise =
    let
      (CastIntersection casts expr') = expr
      casts' = filter (\c -> elem (finalType c) $ getInstancesType typ) casts
    in CastIntersection casts' expr'

-- substitute types in annotations and type information in all terms
-- using the substitutions generated during constraint unification
substituteTypedExpression :: TypeSubstitutions -> Expression -> Expression
substituteTypedExpression s = mapExpression (substituteTypedExpression' s)

-- substitute types in annotations and type information
-- using the substitutions generated during constraint unification
substituteTypedExpression' :: TypeSubstitutions -> Expression -> Expression
substituteTypedExpression' s (Variable var typ) =
    Variable var (foldr substituteType typ $ reverse s)
substituteTypedExpression' s (AnnotatedAbstraction var typ expr) =
    AnnotatedAbstraction var (foldr substituteType typ $ reverse s) expr
substituteTypedExpression' s (TypeInformation typ expr) =
    TypeInformation (foldr substituteType typ $ reverse s) expr
substituteTypedExpression' s e = e

-- HELPER FUNCTIONS

-- remove type information from all terms in expression
removeTypeInformation :: Expression -> Expression
removeTypeInformation = mapExpression removeTypeInformation'

-- remove type information from expression
removeTypeInformation' :: Expression -> Expression
removeTypeInformation' (TypeInformation _ expr) = expr
removeTypeInformation' e = e

-- remove repeated instances of intersection types from expression
removeRepeatedInstancesExpr :: Expression -> Expression
removeRepeatedInstancesExpr = mapExpression removeRepeatedInstancesExpr'

-- remove repeated instances of intersection types from expression
removeRepeatedInstancesExpr' :: Expression -> Expression
removeRepeatedInstancesExpr' (TypeInformation typ e) = TypeInformation (removeRepeatedInstances typ) e
removeRepeatedInstancesExpr' (Variable var typ) = Variable var (removeRepeatedInstances typ)
removeRepeatedInstancesExpr' e = e

-- remove identity casts from all terms in expression
removeIdentityCasts :: Expression -> Expression
removeIdentityCasts = mapExpression removeIdentityCasts'

-- remove identity cast from expression
removeIdentityCasts' :: Expression -> Expression
removeIdentityCasts' expr@(CastIntersection cs expr')
    -- remove cast intersection with only empty casts
    | all isEmptyCast cs' = expr'
    | otherwise = CastIntersection cs' expr'
    -- remove identity casts
    where cs' = map removeIdentityCastsI' cs
removeIdentityCasts' e = e

-- remove identity casts in all cast intersection
removeIdentityCastsI :: Cast -> Cast
removeIdentityCastsI = mapCast removeIdentityCastsI'

-- remove identity casts in cast intersection
removeIdentityCastsI' :: Cast -> Cast
removeIdentityCastsI' c@(SingleCast _ t1 t2 c')
    -- if is identity cast, remove it
    | t1 == t2 = c'
    | otherwise = c
removeIdentityCastsI' c = c

{-
-- get int from expression
getExpectedInt :: Expression -> Expression
-- if expression is an integer, return it
getExpectedInt e@(Int i) = e
-- if expression is a cast intersection
getExpectedInt (CastIntersection cs e)
    -- and if there is an empty cast, remove casst intersection
    | any isEmptyCast cs = getExpectedInt e
    -- if there is no empty cast, then there must be a blame to propagate
    | any isBlameCast cs =
        let (BlameCast _ _ f msg) = head $ filter isBlameCast cs
        in Blame f msg
-}
