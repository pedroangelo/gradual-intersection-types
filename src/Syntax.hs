module Syntax where

-- Types
import Types

-- Imports
import Data.List
import Control.Monad.State

-- Expressions in λ-calculus and extensions
data Expression
    -- Typed λ-calculus terms
    = Variable Var
    | Abstraction Var Type Expression
    | Application Expression Expression
    -- Integers
    | Int Int
    -- Booleans
    | Bool Bool
    -- Arithmetic operator
    | Addition Expression Expression
    -- Type annotations
    | TypeInformation Type Expression
    -- Intersection of casts
    | IntersectionCasts [CastI] Expression
    -- Blames
    | Blame Type Message
    deriving (Show, Eq)

-- Cast inhabiting intersection of casts
data CastI
    = SingleCast CastLabel Type Type CastI
    | BlameCast CastLabel Type Type Message
    | EmptyCast CastLabel Type
    deriving (Show, Eq)

type CastLabel = Int

-- MAPPING

-- Expression Mapping
mapExpression :: (Expression -> Expression) -> Expression -> Expression

-- Typed λ-calculus terms
mapExpression f e@(Variable var) = f e
mapExpression f e@(Abstraction var typ expr) =
    f (Abstraction var typ $ mapExpression f expr)
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

-- Intersection of Casts
mapExpression f e@(IntersectionCasts cs expr) =
    f (IntersectionCasts cs (mapExpression f expr))

-- Blames
mapExpression f e@(Blame typ label) = f e

-- Expression Mapping
mapCast :: (CastI -> CastI) -> CastI -> CastI
mapCast f (SingleCast cl t1 t2 cs) =
    f (SingleCast cl t1 t2 $ mapCast f cs)
mapCast f c@BlameCast{} = f c
mapCast f c@EmptyCast{} = f c

-- CHECKS

-- check if it's a variable
isVariable :: Expression -> Bool
isVariable Variable{} = True
isVariable _ = False

-- check if it's an abstraction
isAbstraction :: Expression -> Bool
isAbstraction Abstraction{} = True
isAbstraction _ = False

-- check if it's an application
isApplication :: Expression -> Bool
isApplication Application{} = True
isApplication _ = False

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

-- check if it's a intersection of casts
isIntersectionCasts :: Expression -> Bool
isIntersectionCasts IntersectionCasts{} = True
isIntersectionCasts _ = False

-- check if it's a blame
isBlame :: Expression -> Bool
isBlame Blame{} = True
isBlame _ = False

-- check if it's a single cast
isSingleCast :: CastI -> Bool
isSingleCast SingleCast{} = True
isSingleCast _ = False

-- check if it's a blame cast
isBlameCast :: CastI -> Bool
isBlameCast BlameCast{} = True
isBlameCast _ = False

-- check if it's an empty cast
isEmptyCast :: CastI -> Bool
isEmptyCast EmptyCast{} = True
isEmptyCast _ = False

-- check if it's a value
isValue :: Expression -> Bool
isValue e =
    isVariable e ||
    isAbstraction e ||
    isBool e ||
    isInt e ||
    isValueIntersectionCasts e ||
    isBlame e

-- check if intersection of casts is a value
isValueIntersectionCasts :: Expression -> Bool
isValueIntersectionCasts (IntersectionCasts cs e) =
    all isCastValue cs &&
    not (all isEmptyCast cs) &&
    not (all isBlameCast cs) &&
    isValue e
isValueIntersectionCasts _ = False

-- check if casts in intersection of casts is a value in the cast semantics
isCastValue :: CastI -> Bool
isCastValue c = isCastValue1 c || isCastValue2 c

-- check if casts in intersection of casts is a value in the cast semantics
isCastValue1 :: CastI -> Bool
isCastValue1 (SingleCast _ t1 t2 c) =
    (isGroundType t1 && isDynType t2 && isCastValue1 c) ||
    (isArrowType t1 && isArrowType t2 && t1 /= t2 && isCastValue1 c) ||
    (isGroundType t1 && isDynType t2 && isEmptyCast c) ||
    (isArrowType t1 && isArrowType t2 && t1 /= t2 && isEmptyCast c)
isCastValue1 _ = False

-- check if casts in intersection of casts is a value in the cast semantics
isCastValue2 :: CastI -> Bool
isCastValue2 c =
    isBlameCast c ||
    isEmptyCast c

-- check if single cast is compatible with arrow type
isArrowCompatible :: CastI -> Bool
isArrowCompatible (SingleCast _ t1 t2 c) = isArrowType t1 && isArrowType t2 && isArrowCompatible c
isArrowCompatible (EmptyCast cl t) = isArrowType t
isArrowCompatible _ = False

-- PROJECTIONS

-- get type from type information
fromTypeInformation :: Expression -> Type
fromTypeInformation (TypeInformation typ _) = typ

-- get cast label
getCastLabel :: CastI -> CastLabel
getCastLabel (SingleCast cl _ _ _) = cl
getCastLabel (BlameCast cl _ _ _) = cl
getCastLabel (EmptyCast cl _) = cl

-- SUBSTITUTIONS
type ExpressionSubstitution = (String, Expression)

-- Substitute expressions according to substitution
substitute :: ExpressionSubstitution -> Expression -> Expression

-- Typed λ-calculus terms
substitute s@(old, new) e@(Variable var)
    -- if var equals old, replace variable with new expression
    | var == old = new
    -- otherwise, do nothing
    | otherwise = e
substitute s@(old, new) e@(Abstraction var typ expr)
    -- if some abstraction has already binded the variable, don't propagate substitutions
    | var == old = e
    -- otherwise, propagate substitutions
    | otherwise = Abstraction var typ $ substitute s expr
substitute s@(old, new) e@(Application expr1 expr2) =
    Application (substitute s expr1) (substitute s expr2)

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

-- Intersection of casts
substitute s@(old, new) e@(IntersectionCasts cs expr) =
    IntersectionCasts cs $ substitute s expr

-- Blames
substitute s@(old, new) e@(Blame t1 label) = e

-- substitute types in annotations and type information in all terms
-- using the substitutions generated during constraint unification
substituteTypedExpression :: TypeSubstitutions -> Expression -> Expression
substituteTypedExpression s = mapExpression (substituteTypedExpression' s)

-- substitute types in annotations and type information
-- using the substitutions generated during constraint unification
substituteTypedExpression' :: TypeSubstitutions -> Expression -> Expression
substituteTypedExpression' s (TypeInformation typ expr) =
    TypeInformation (foldr substituteType typ s) expr
substituteTypedExpression' s e = e

-- HELPER FUNCTIONS

-- remove type information from all terms in expression
removeTypeInformation :: Expression -> Expression
removeTypeInformation = mapExpression removeTypeInformation'

-- remove type information from expression
removeTypeInformation' :: Expression -> Expression
removeTypeInformation' (TypeInformation _ expr) = expr
removeTypeInformation' e = e

-- remove identity casts from all terms in expression
removeIdentityCasts :: Expression -> Expression
removeIdentityCasts = mapExpression removeIdentityCasts'

-- remove identity cast from expression
removeIdentityCasts' :: Expression -> Expression
removeIdentityCasts' expr@(IntersectionCasts cs expr')
    -- remove intersection cast with only empty casts
    | all isEmptyCast cs' = expr'
    | otherwise = IntersectionCasts cs' expr'
    -- remove identity casts
    where cs' = map removeIdentityCastsI' cs
removeIdentityCasts' e = e

-- remove identity casts in all intersection casts
removeIdentityCastsI :: CastI -> CastI
removeIdentityCastsI = mapCast removeIdentityCastsI'

-- remove identity casts in intersection casts
removeIdentityCastsI' :: CastI -> CastI
removeIdentityCastsI' c@(SingleCast _ t1 t2 c')
    -- if is identity cast, remove it
    | t1 == t2 = c'
    | otherwise = c
removeIdentityCastsI' c = c

-- get initial type
initialType :: CastI -> Type
initialType (SingleCast cl t1 t2 c) = initialType c
initialType (BlameCast cl i f msg) = i
initialType (EmptyCast cl t) = t

-- get final type
finalType :: CastI -> Type
finalType (SingleCast cl t1 t2 c) = t2
finalType (BlameCast cl i f msg) = f
finalType (EmptyCast cl t) = t

-- Merge intersection cast with intersection cast
mergeCasts :: Expression -> Expression
mergeCasts (IntersectionCasts cs (IntersectionCasts cs' expr)) =
    -- merge casts according to cast labels, only merging casts whose labels are comparable
    -- (this enables casts generated by the simulate step to only merge with their corresponding casts from the original cast),
    -- and according to types, only merging casts that ensure type safety
    -- (this enables merged casts to always be able to be reduced)
    IntersectionCasts [joinCastI y x | x <- cs', y <- cs, isSameCastLabel y x && initialType y == finalType x] expr

-- compare cast labels between casts
isSameCastLabel :: CastI -> CastI -> Bool
isSameCastLabel c1 c2
    -- suceed only if there is no cast label in any of the casts
    | getCastLabel c1 == 0 || getCastLabel c2 == 0 = True
    -- or if the cast label is the same
    | otherwise = getCastLabel c1 == getCastLabel c2

-- join casts in intersections
joinCastI :: CastI -> CastI -> CastI
joinCastI (SingleCast cl t1 t2 c) cast = SingleCast cl t1 t2 $ joinCastI c cast
joinCastI (BlameCast cl i f msg) cast = BlameCast cl i f msg
joinCastI (EmptyCast cl t) cast = assignCastLabel cl cast

-- Apply simulate step
simulateArrow :: Expression -> Expression -> Expression
simulateArrow (IntersectionCasts cs expr1') expr2 =
    let
        -- filter arrow compatible casts
        compatibleCasts = filter isArrowCompatible cs
        -- clean cast labels
        cleanCasts = map (mapCast cleanCastLabel) compatibleCasts
        -- obtain first intersection casts that will be used in simulate step
        (firstCasts, secondCasts) = unzip $ map separateIntersectionCast cleanCasts
        -- assign cast label based in position
        firstCasts' = map (\x -> mapCast (assignCastLabel $ fst x) (snd x)) $ zip [1..] firstCasts
        -- breakdown arrow types in casts to form two casts
        (cs1, cs2) = unzip $ map breakdownArrowType firstCasts'
    -- simulate casts on arrow types
    in IntersectionCasts cs2 $ Application (IntersectionCasts secondCasts expr1') (IntersectionCasts cs1 expr2)

-- remove first cast from intersection cast
separateIntersectionCast :: CastI -> (CastI, CastI)
separateIntersectionCast (SingleCast cl t1 t2 c) = (SingleCast cl t1 t2 $ EmptyCast cl t1, c)
separateIntersectionCast (EmptyCast cl t) = (EmptyCast cl t, EmptyCast cl t)

-- group the components of arrow types in the cast
breakdownArrowType :: CastI -> (CastI, CastI)
-- Separate the cast T11 -> T12 => T21 -> T22,
breakdownArrowType (SingleCast cl (ArrowType t11 t12) (ArrowType t21 t22) (EmptyCast _ _)) =
    -- in two casts: T21 => T11 and T12 => T22
    (SingleCast cl t21 t11 $ EmptyCast cl t21, SingleCast cl t12 t22 $ EmptyCast cl t12)
-- If cast is an empty cast, just return two empty casts
breakdownArrowType (EmptyCast cl (ArrowType t1 t2)) = (EmptyCast cl t1, EmptyCast cl t2)

-- clean cast labels
cleanCastLabel :: CastI -> CastI
cleanCastLabel = assignCastLabel 0

-- assign cast label to cast
assignCastLabel :: CastLabel -> CastI -> CastI
assignCastLabel cl (SingleCast _ t1 t2 cs) = SingleCast cl t1 t2 $ assignCastLabel cl cs
assignCastLabel cl (BlameCast _ i f msg) = BlameCast cl i f msg
assignCastLabel cl (EmptyCast _ t) = EmptyCast cl t

-- get int from expression
getExpectedInt :: Expression -> Expression
-- if expression is an integer, return it
getExpectedInt e@(Int i) = e
-- if expression is a intersection of casts
getExpectedInt (IntersectionCasts cs e)
    -- and if there is an empty cast, remove intersection of casts
    | any isEmptyCast cs = getExpectedInt e
    -- if there is no empty cast, thenthere must be a blame to propagate
    | any isBlameCast cs =
        let (BlameCast _ _ f msg) = head $ filter isBlameCast cs
        in Blame f msg

-- generate casts according to the number of instances of types
addCasts :: [Type] -> [Type] -> Expression -> Expression
addCasts t1 t2 e
    | length t1 == 1 && length t2 == 1 = IntersectionCasts [SingleCast 0 (head t1) (head t2) $ EmptyCast 0 (head t1)] e
    | length t1 == length t2 = IntersectionCasts (map (\x -> SingleCast 0 (fst x) (snd x) $ EmptyCast 0 (fst x)) $ zip t1 t2) e
    | length t1 == 1 || length t2 == 1 = IntersectionCasts [SingleCast 0 x y $ EmptyCast 0 x | x <- t1, y <- t2] e
