module CastInsertion (
  insertCasts
) where

-- Syntax & Types
import Syntax
import Types

-- Imports
import Data.Maybe

-- insert casts in the expression
insertCasts :: Expression -> Expression

-- if expression is a value
insertCasts e@(TypeInformation _ (Variable _ _)) = e

-- if expression is an annotated abstraction
insertCasts e@(TypeInformation typ (AnnotatedAbstraction var t expr)) =
  TypeInformation typ $ AnnotatedAbstraction var t $ insertCasts expr

-- if expression is an application
insertCasts e@(TypeInformation typ (Application expr1 expr2)) =
  let
    -- insert casts
    expr1' = insertCasts expr1
    expr2' = insertCasts expr2
    -- build types
    TypeInformation t1 _ = expr1'
    TypeInformation t2 _ = expr2'
    d1 = patternMatchArrow t1
    ArrowType d2 t = d1
    -- build casts
    cast1 = addCasts (getInstancesType t1) (getInstancesType d1) expr1'
    cast2 = addCasts (getInstancesType t2) (getInstancesType d2) expr2'
  in TypeInformation typ $ Application cast1 cast2

-- if expression is an abstraction
insertCasts e@(TypeInformation typ (Abstraction var expr)) =
  TypeInformation typ $ Abstraction var $ insertCasts expr

-- if expression is an integer
insertCasts e@(TypeInformation _ (Int _)) = e

-- if expression is a boolean
insertCasts e@(TypeInformation _ (Bool _)) = e

-- if expression is an addition operation
insertCasts e@(TypeInformation typ (Addition expr1 expr2)) =
  let
    -- insertCasts
    expr1' = insertCasts expr1
    expr2' = insertCasts expr2
    -- build types
    TypeInformation t1 _ = expr1'
    TypeInformation t2 _ = expr2'
    -- build casts
    cast1 = addCasts (getInstancesType t1) [IntType] expr1'
    cast2 = addCasts (getInstancesType t2) [IntType] expr2'
  in TypeInformation typ $ Addition cast1 cast2

-- obtain pattern match type for arrow
patternMatchArrow :: Type -> Type
patternMatchArrow e@(ArrowType _ _) = e
patternMatchArrow DynType = ArrowType DynType DynType

-- generate casts according to the number of instances of types
addCasts :: [Type] -> [Type] -> Expression -> Expression
addCasts t1 t2 e
  | length t1 == 1 && length t2 == 1 = CastIntersection [SingleCast 0 (head t1) (head t2) $ EmptyCast 0 (head t1)] e
  | length t1 == length t2 = CastIntersection (map (\x -> SingleCast 0 (fst x) (snd x) $ EmptyCast 0 (fst x)) $ zip t1 t2) e
  | length t1 == 1 || length t2 == 1 = CastIntersection [SingleCast 0 x y $ EmptyCast 0 x | x <- t1, y <- t2] e
