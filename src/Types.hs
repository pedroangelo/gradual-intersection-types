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
  | IntersectionType [Type]
  deriving (Show, Eq)

-- Constraints
type Constraints = [Constraint]
data Constraint
  = Equality Type Type
  -- | Consistency Type Type
  | Inequality Type Type
  | Choice Type Type
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
mapType f t@(IntersectionType ts) =
    f (IntersectionType $ map (mapType f) ts)

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

-- check is both types are the equal modulo associativity, commutativity and idempotence
sameType :: Type -> Type -> Bool
sameType t1 t2
  | t1 == t2 = True
  | isIntersectionType t1 || isIntersectionType t2 = null (instances1 \\ instances2) && null (instances2 \\ instances1)
  where instances1 = getInstancesType $ removeRepeatedInstances t1
        instances2 = getInstancesType $ removeRepeatedInstances t2

-- PROJECTIONS

-- get ground type
getGroundType :: Type -> Type
getGroundType ArrowType{} = ArrowType DynType DynType
getGroundType IntType = IntType
getGroundType BoolType = BoolType

-- get first constraint
getFirstConstraint :: Constraint -> Type
getFirstConstraint (Equality t _) = t
getFirstConstraint (Inequality t _) = t
getFirstConstraint (Choice t _) = t

-- get second constraint
getSecondConstraint :: Constraint -> Type
getSecondConstraint (Equality _ t) = t
getSecondConstraint (Inequality _ t) = t
getSecondConstraint (Choice _ t) = t

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
substituteType s@(old, new) t@(IntersectionType ts) =
  IntersectionType $ map (substituteType s) ts

-- apply substitution to constraints
substituteConstraint :: TypeSubstitution -> Constraint -> Constraint
substituteConstraint s (Equality t1 t2) =
  Equality (substituteType s t1) (substituteType s t2)
-- substituteConstraint s (Consistency t1 t2) =
   --  Consistency (substituteType s t1) (substituteType s t2)
substituteConstraint s (Inequality t1 t2) =
  Inequality (substituteType s t1) (substituteType s t2)
substituteConstraint s (Choice t1 t2) =
  Choice (substituteType s t1) (substituteType s t2)

-- HELPER FUNCTIONS

-- build new type variable
newTypeVar :: Int -> Type
newTypeVar index = VarType ("t" ++ show index)

-- collect all type variables
collectTypeVariables :: Type -> [String]
collectTypeVariables (VarType var) = [var]
collectTypeVariables IntType = []
collectTypeVariables BoolType = []
collectTypeVariables DynType = []
collectTypeVariables (ArrowType t1 t2) =
  collectTypeVariables t1 ++ collectTypeVariables t2
collectTypeVariables (IntersectionType ts) =
  concatMap collectTypeVariables ts


-- get instances of intersection type
getInstancesType :: Type -> [Type]
getInstancesType (VarType var) = [VarType var]
getInstancesType IntType = [IntType]
getInstancesType BoolType = [BoolType]
getInstancesType DynType = [DynType]
getInstancesType (ArrowType t1 t2) =
  let t1' = getInstancesType t1
      t2' = getInstancesType t2
  in [ArrowType x y | x <- t1', y <- t2']
getInstancesType (IntersectionType ts) =
  concatMap getInstancesType ts

-- remove repeated instances from intersection type
removeRepeatedInstances :: Type -> Type
removeRepeatedInstances = mapType removeRepeatedInstances'

-- remove repeated instances from intersection type
removeRepeatedInstances' :: Type -> Type
removeRepeatedInstances' (IntersectionType instances)
  | length new_instances == 1 = head new_instances
  | otherwise = IntersectionType new_instances
  where new_instances = nub instances
removeRepeatedInstances' e = e
