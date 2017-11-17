module Syntax where

-- Types
import Types

-- Imports
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
    -- Casts
    | Cast Type Type Expression
    -- Intersection of casts
    | IntersectionCasts [CastI] Expression
    -- Blames
    | Blame Type Message
    deriving (Show, Eq)

-- Cast inhabiting intersection of casts
data CastI
    = SingleCast CastLabel Type Type CastI
    | BlameCast CastLabel Type Message
    | EmptyCast CastLabel
    | StuckCast CastLabel
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

-- Casts
mapExpression f e@(Cast type1 type2 expr) =
    f (Cast type1 type2 (mapExpression f expr))
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
mapCast f c@StuckCast{} = f c

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

-- check if it's a cast
isCast :: Expression -> Bool
isCast Cast{} = True
isCast _ = False

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

-- check if it's a stuck cast
isStuckCast :: CastI -> Bool
isStuckCast StuckCast{} = True
isStuckCast _ = False

-- check if it's a value
isValue :: Expression -> Bool
isValue e =
    isVariable e ||
    isAbstraction e ||
    isBool e ||
    isInt e ||
    isValueCast e ||
    isValueIntersectionCasts e ||
    isBlame e

-- check if cast is a value
isValueCast :: Expression -> Bool
isValueCast (Cast t1 t2 e) =
    (isGroundType t1 && isDynType t2 && isValue e) ||
    (isArrowType t1 && isArrowType t2 && isValue e && t1 /= t2)
isValueCast _ = False

-- check if intersection of casts is a value
isValueIntersectionCasts :: Expression -> Bool
isValueIntersectionCasts (IntersectionCasts cs e) =
    (all isValueCastI cs && isValue e) ||
    (all (\x -> isValueCastI x || isEmptyCast x || isBlameCast x) cs && not (all isEmptyCast cs) && not (all isBlameCast cs) && isValue e)
isValueIntersectionCasts _ = False

-- check if cast in intersection of casts is a value
isValueCastI :: CastI -> Bool
isValueCastI (SingleCast _ t1 t2 c) =
    (isGroundType t1 && isDynType t2 && isValueCastI c) ||
    (isArrowType t1 && isArrowType t2 && t1 /= t2 && isValueCastI c) ||
    (isGroundType t1 && isDynType t2 && isEmptyCast c) ||
    (isArrowType t1 && isArrowType t2 && t1 /= t2 && isEmptyCast c)
isValueCastI _ = False

-- check if single cast is compatible with arrow type
isArrowCompatible :: CastI -> Bool
isArrowCompatible (SingleCast _ t1 t2 c) = isArrowType t1 && isArrowType t2 && isArrowCompatible c
isArrowCompatible (EmptyCast _) = True
isArrowCompatible _ = False

-- PROJECTIONS

-- get type from type information
fromTypeInformation :: Expression -> Type
fromTypeInformation (TypeInformation typ _) = typ

-- get cast types
getCastTypes :: Expression -> (Type, Type)
getCastTypes (Cast t1 t2 _) = (t1, t2)

-- get cast label
getCastLabel :: CastI -> CastLabel
getCastLabel (SingleCast cl _ _ _) = cl
getCastLabel (BlameCast cl _ _) = cl
getCastLabel (EmptyCast cl) = cl
getCastLabel (StuckCast cl) = cl

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

-- Casts
substitute s@(old, new) e@(Cast t1 t2 expr) =
    Cast t1 t2 $ substitute s expr

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
removeIdentityCasts' expr@(Cast t1 t2 expr')
    -- if is identity cast, remove it
    | t1 == t2 = expr'
    | otherwise = expr
removeIdentityCasts' expr@(IntersectionCasts cs expr')
    | all isEmptyCast cs' = expr'
    | otherwise = IntersectionCasts cs' expr'
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

-- convert intersection cast to simple cast
convertToSimpleCast :: CastI -> Expression -> Expression
convertToSimpleCast (SingleCast _ t1 t2 c) expr = Cast t1 t2 $ convertToSimpleCast c expr
convertToSimpleCast (BlameCast _ t msg) expr = Blame t msg
convertToSimpleCast (EmptyCast _) expr = expr

-- convert casts to IntersectionCasts
convertToIntersectionCasts :: Expression -> Expression
convertToIntersectionCasts (Cast t1 t2 expr) = IntersectionCasts [SingleCast 0 t1 t2 $ EmptyCast 0] expr

-- merge casts recursively until sub expression is not a cast
mergeCasts :: Expression -> Expression
mergeCasts e@(IntersectionCasts cs expr)
    -- if merging two intersection of casts
    | isIntersectionCasts expr =
        let IntersectionCasts cs' expr' = expr
            -- merge casts according to cast labels, only merging casts whose labels are comparable
            -- this enables casts generated by the simulate step to only merge with their corresponding casts from the original cast
            intersectionCasts = concatMap (\x -> concatMap (\y -> [joinCastI y x | isSameCastLabel y x]) cs) cs'
        in mergeCasts $ IntersectionCasts intersectionCasts expr'
    -- if merging an intersection cast with a cast
    | isCast expr =
        -- convert cast to intersection of casts with just one cast
        let (IntersectionCasts cs' expr') = convertToIntersectionCasts expr
        -- merge the single cast with each intersection cast
        in mergeCasts $ IntersectionCasts (map (\x -> joinCastI x (head cs')) cs) expr'
    | otherwise = e

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
joinCastI (BlameCast cl t msg) cast = BlameCast cl t msg
joinCastI (EmptyCast cl) cast = assignCastLabel cl cast
joinCastI (StuckCast cl) cast = StuckCast cl

-- remove first cast from intersection cast
separateIntersectionCast :: CastI -> (CastI, CastI)
separateIntersectionCast (SingleCast cl t1 t2 c) = (SingleCast cl t1 t2 $ EmptyCast cl, c)
separateIntersectionCast (EmptyCast cl) = (EmptyCast cl, EmptyCast cl)

-- separate first casts while removing incompatible casts
getArrowCompatibleCasts :: Expression -> Expression
getArrowCompatibleCasts (IntersectionCasts cs expr) =
    let
        -- filter all arrow compatible casts
        compatibleCasts = filter isArrowCompatible cs
        -- obtain first intersection casts that will be used in simulate step
        (firstCasts, secondCasts) = unzip $ map separateIntersectionCast compatibleCasts
        -- filter empty intersection of casts
        result
            | all isEmptyCast compatibleCasts = expr
            | all isEmptyCast secondCasts = IntersectionCasts firstCasts expr
            | otherwise = IntersectionCasts firstCasts $ IntersectionCasts secondCasts expr
    in result

-- group the components of arrow types in the cast
getCompatibleArrowType :: CastI -> (CastI, CastI)
-- Separate the cast T11 -> T12 => T21 -> T22,
getCompatibleArrowType (SingleCast cl (ArrowType t11 t12) (ArrowType t21 t22) (EmptyCast _)) =
    -- in two casts: T21 => T11 and T12 => T22
    (SingleCast cl t21 t11 $ EmptyCast cl, SingleCast cl t12 t22 $ EmptyCast cl)
-- If cast is an empty cast, just return two empty casts
getCompatibleArrowType (EmptyCast cl) = (EmptyCast cl, EmptyCast cl)

-- simulate casts on arrow data type
simulateCastsArrow :: Expression -> Expression -> Expression
simulateCastsArrow (IntersectionCasts cs expr1') expr2 =
    -- get intersection casts for each position, assigning cast labels according to position of intersection cast in intersection of casts
    let (cs1, cs2) = unzip $
            map (\x -> (mapCast (assignCastLabel $ fst x) $ fst $ snd x, mapCast (assignCastLabel $ fst x) $ snd $ snd x)) $
            zip [1..] $ map getCompatibleArrowType cs
    in IntersectionCasts cs2 $ Application expr1' (IntersectionCasts cs1 expr2)

-- clean cast labels
cleanCastLabel :: CastI -> CastI
cleanCastLabel = assignCastLabel 0

-- assign cast label to cast
assignCastLabel :: CastLabel -> CastI -> CastI
assignCastLabel cl (SingleCast _ t1 t2 cs) = SingleCast cl t1 t2 $ assignCastLabel cl cs
assignCastLabel cl (BlameCast _ t msg) = BlameCast cl t msg
assignCastLabel cl (EmptyCast _) = EmptyCast cl
assignCastLabel cl (StuckCast _) = StuckCast cl

-- get int from expression
getExpectedInt :: Expression -> Expression
-- if expression is an integer, return it
getExpectedInt e@(Int i) = e
-- if expression is a intersection of casts
getExpectedInt (IntersectionCasts cs e)
    -- and if there is an empty cast, remove intersection of casts
    | any isEmptyCast cs = getExpectedInt e
    -- if there is no empty cast, thenthere must be a blame to propagate
    | any isBlameCast cs = convertToSimpleCast (head $ filter isBlameCast cs) e

-- generate casts according to the number of instances of types
addCasts :: [Type] -> [Type] -> Expression -> Expression
addCasts t1 t2 e
    | length t1 == 1 && length t2 == 1 = Cast (head t1) (head t2) e
    | length t1 == length t2 = IntersectionCasts (map (\x -> SingleCast 0 (fst x) (snd x) $ EmptyCast 0) $ zip t1 t2) e
    | length t1 == 1 || length t2 == 1 = IntersectionCasts [SingleCast 0 x y $ EmptyCast 0 | x <- t1, y <- t2] e
