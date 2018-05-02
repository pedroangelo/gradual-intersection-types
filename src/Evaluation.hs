module Evaluation (
    evaluate,
    evaluateCasts
) where

-- Syntax & Types
import Syntax
import Types

-- Imports
import Data.Maybe
import Control.Monad.State

evaluationStyle = id
evaluateCastStyle = id

-- evaluate using call-by-value strategy
evaluate :: Expression -> Expression

-- variables are values
evaluate e@Variable{} = e

-- abstractions are values
evaluate e@Abstraction{} = e

-- if expression is an application
evaluate e@(Application expr1 expr2)
    -- push blames to top level
    | isBlame expr1 = expr1
    | isBlame expr2 = expr2
    -- reduce expr1
    | not $ isValue expr1 =
        let v1 = evaluate expr1
        in evaluationStyle $ Application v1 expr2
    -- reduce expr2
    | not $ isValue expr2 =
        let v2 = evaluate expr2
        in evaluationStyle $ Application expr1 v2
    -- simulate casts on data types
    | isValue expr1 && isValue expr2 && isCastIntersection expr1 && any isArrowCompatible cs =
        evaluationStyle $ simulateArrow expr1 expr2
    -- beta reduction
    | isAbstraction expr1 && isValue expr2 =
        let (Abstraction var typ expr) = expr1
        in evaluationStyle $ substitute (var, expr2) expr
    where CastIntersection cs expr1'' = expr1

-- booleans are values
evaluate e@Bool{} = e

-- integers are values
evaluate e@Int{} = e

-- if expression is a addition
evaluate e@(Addition expr1 expr2)
    -- push blames to top level
    | isBlame expr1 = expr1
    | isBlame expr2 = expr2
    -- reduce expr1
    | not $ isValue expr1 =
        let v1 = evaluate expr1
        in evaluationStyle $ Addition v1 expr2
    -- reduce expr2
    | not $ isValue expr2 =
        let v2 = evaluate expr2
        in evaluationStyle $ Addition expr1 v2
    -- call native addition function between expr1 and expr2
    | isInt expr1 && isInt expr2 =
        let
            Int i1 = expr1
            Int i2 = expr2
        in Int $ i1 + i2
    -- remove remaining casts such as empty casts and blames
    -- | otherwise = evaluationStyle $ Addition (getExpectedInt expr1) (getExpectedInt expr2)

-- if expression is a type information
evaluate e@(TypeInformation typ expr) = expr

-- if expression is an cast intersection
evaluate e@(CastIntersection cs expr)
    -- push blame to top level
    | isBlame expr = evaluationStyle expr
    -- merge cast intersection with cast intersection
    | isValue expr && isCastIntersection expr = evaluationStyle $ mergeCasts e
    -- values don't reduce
    | isValue e = e
    -- evaluate inside a cast
    | not (isValue expr) =
        let expr2 = evaluate expr
        in evaluationStyle $ CastIntersection cs expr2
    -- evaluate casts in intersections
    | isValue expr && not (all isCastValue cs) =
        evaluationStyle $ CastIntersection (map evaluateCasts cs) expr
    -- propagate blame
    | isValue expr && all isBlameCast cs =
        let (BlameCast _ i f msg) = head cs
        in evaluationStyle $ Blame (IntersectionType $ map (\x -> let (BlameCast cl i f msg) = x in f) cs) msg
    -- remove empty casts
    | isValue expr && all isEmptyCast cs = evaluationStyle expr

-- blames are values
evaluate e@Blame{} = e

-- evaluate casts in intersections
evaluateCasts :: Cast -> Cast

-- evaluate single casts
evaluateCasts c@(SingleCast cl t1 t2 c1)
    -- push blames to top level
    | isBlameCast c1 =
        let (BlameCast cl' i _ msg) = c1
        in BlameCast cl' i t2 msg
    -- values don't reduce
    | isCastValue c = c
    -- evaluate inside cast
    | not (isCastValue c1) =
        let c1' = evaluateCasts c1
        in evaluateCastStyle $ SingleCast cl t1 t2 c1'
    -- ID-BASE - remove casts to same types
    | (isCastValue1 c1 || isEmptyCast c1) && t1 == t2 = evaluateCastStyle c1
    -- SUCCEED - cast is sucessful
    | (isCastValue1 c1 || isEmptyCast c1) && isSingleCast c1 && t1 == DynType && t2' == DynType && isGroundType t2 && t1' == t2 =
            evaluateCastStyle c'
    -- FAIL - cast fails
    | (isCastValue1 c1 || isEmptyCast c1) && isSingleCast c1 && t1 == DynType && t2' == DynType &&
        isGroundType t2 && isGroundType t1' && not (sameGround t1' t2) =
            BlameCast (getCastLabel c1) (initialType c') t2 $ "cannot cast from " ++ show t1' ++ " to " ++ show t2
    -- GROUND - cast types through their ground types
    | (isCastValue1 c1 || isEmptyCast c1) && not (isGroundType t1) && t2 == DynType =
        let g = getGroundType t1
        in evaluateCastStyle $ SingleCast cl g DynType $ SingleCast cl t1 g c1
    -- EXPAND - cast types through their ground types
    | (isCastValue1 c1 || isEmptyCast c1) && not (isGroundType t2) && t1 == DynType =
        let g = getGroundType t2
        in evaluateCastStyle $ SingleCast cl g t2 $ SingleCast cl DynType g c1
    -- Project types and expression from inner casts
    where (SingleCast cl' t1' t2' c') = c1

-- blame cast evaluates to itself
evaluateCasts c@BlameCast{} = c

-- empty cast evaluates to itself
evaluateCasts c@EmptyCast{} = c
