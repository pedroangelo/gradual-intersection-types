module Types where

-- Imports
import Data.Char
import Data.List

-- Context holds bindings between variables and types
type Context = [Bindings]
type Bindings = (Var, Type)

-- Types in λ-calculus and extensions
data Type
    -- Type variable: Var
    = VarType Var
    -- Arrow type: Type -> Type
    | ArrowType Type Type
    -- Integer type: Int
    | IntType
    -- Boolean type: Bool
    | BoolType
    -- Dynamic type: Dyn
    | DynType
    -- Intersection type: Type /\ Type
    | IntersectionType Type Type
    deriving (Show, Eq)

type Var = String
type Message = String

-- MAPPING

-- Type Mapping
mapType :: (Type -> Type) -> Type -> Type

-- Type variable
mapType f t@(VarType var) = f t

-- Arrow type
mapType f t@(ArrowType t1 t2) =
    f (ArrowType (mapType f t1) (mapType f t2))

-- Integer type
mapType f t@IntType = f t

-- Boolean type
mapType f t@BoolType = f t

-- Dynamic type
mapType f t@DynType = f t

-- Product type
mapType f t@(IntersectionType t1 t2) =
    f (IntersectionType (mapType f t1) (mapType f t2))

-- CHECKS

-- check if it's a variable type
isVarType :: Type -> Bool
isVarType VarType{} = True
isVarType _ = False

-- check if it's a function type
isArrowType :: Type -> Bool
isArrowType ArrowType{} = True
isArrowType _ = False

-- check if it's an integer type
isIntType :: Type -> Bool
isIntType IntType = True
isIntType _ = False

-- check if it's a boolean type
isBoolType :: Type -> Bool
isBoolType BoolType = True
isBoolType _ = False

-- check if it's a dynamic type
isDynType :: Type -> Bool
isDynType DynType = True
isDynType _ = False

-- check if it's an intersection type
isIntersectionType :: Type -> Bool
isIntersectionType IntersectionType{} = True
isIntersectionType _ = False

-- check if it's a ground type
isGroundType :: Type -> Bool
isGroundType (ArrowType DynType DynType) = True
isGroundType IntType = True
isGroundType BoolType = True
isGroundType _ = False

-- check if types have the same ground type
sameGround :: Type -> Type -> Bool
sameGround t1 t2 = getGroundType t1 == getGroundType t2

-- PROJECTIONS

-- get ground type
getGroundType :: Type -> Type
getGroundType ArrowType{} = ArrowType DynType DynType
getGroundType IntType = IntType
getGroundType BoolType = BoolType

-- SUBSTITUTIONS
type TypeSubstitutions = [TypeSubstitution]
type TypeSubstitution = (Type, Type)

-- subsitute type
substituteType :: TypeSubstitution -> Type -> Type

-- Type variable
substituteType s@(old, new) t@(VarType var)
    | old == t = new
    | otherwise = t

-- Arrow type
substituteType s@(old, new) t@(ArrowType t1 t2) =
    ArrowType (substituteType s t1) (substituteType s t2)

-- Integer type
substituteType s@(old, new) t@IntType = t

-- Boolean type
substituteType s@(old, new) t@BoolType = t

-- Dynamic type
substituteType s@(old, new) t@DynType = t

-- Intersection type
substituteType s@(old, new) t@(IntersectionType t1 t2) =
    IntersectionType (substituteType s t1) (substituteType s t2)

-- HELPER FUNCTIONS

-- build new type variable
newTypeVar :: Int -> Type
newTypeVar index = VarType ("t" ++ show index)

-- get instances of intersection type
getInstancesType :: Type -> [Type]
getInstancesType IntType = [IntType]
getInstancesType BoolType = [BoolType]
getInstancesType DynType = [DynType]
getInstancesType (ArrowType t1 t2) =
    let t1' = getInstancesType t1
        t2' = getInstancesType t2
    in [ArrowType x y | x <- t1', y <- t2']
getInstancesType (IntersectionType t1 t2) =
    let t1' = getInstancesType t1
        t2' = getInstancesType t2
    in t1' ++ t2'
