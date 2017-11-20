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

evaluationStyle = evaluate
evaluateCastStyle = evaluateCasts

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
    -- C-BETA - simulate casts on data types
    | isValue expr1 && isValue expr2 && isCast expr1 && isArrowType t1 && isArrowType t2 =
        let ArrowType t11 t12 = t1
            ArrowType t21 t22 = t2
            expr2' = Cast t21 t11 expr2
        in evaluationStyle $ Cast t12 t22 $ Application expr1' expr2'
    -- C-BETA/\ - simulate casts on data types
    | isValue expr1 && isValue expr2 && isIntersectionCasts expr1 && any isArrowCompatible cs =
        let
            -- filter arrow compatible casts, and separate first casts from second casts
            compatibleExpr = getArrowCompatibleCasts $ IntersectionCasts (map (mapCast cleanCastLabel) cs) expr1''
            result
                -- apply simulate step if there is a cast
                | isIntersectionCasts compatibleExpr || isCast compatibleExpr = simulateCastsArrow compatibleExpr expr2
                -- otherwise just reduce to a simple application
                | otherwise = evaluationStyle $ Application compatibleExpr expr2
        in evaluationStyle result
    -- beta reduction
    | isAbstraction expr1 && isValue expr2 =
        let (Abstraction var typ expr) = expr1
        in evaluationStyle $ substitute (var, expr2) expr
    where Cast t1 t2 expr1' = expr1
          IntersectionCasts cs expr1'' = expr1

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
    | otherwise = evaluationStyle $ Addition (getExpectedInt expr1) (getExpectedInt expr2)

-- if expression is a type information
evaluate e@(TypeInformation typ expr) = expr

-- if expression is a cast
evaluate e@(Cast t1 t2 expr)
    -- push blame to top level
    | isBlame expr = expr
    -- values don't reduce
    | isValue e = e
    -- evaluate inside a cast
    | not (isValue expr) =
        let expr2 = evaluate expr
        in evaluationStyle $ Cast t1 t2 expr2
    -- ID-BASE - remove casts to same types
    | isValue expr && t1 == t2 = evaluationStyle expr
    -- SUCCEED - cast is sucessful
    | isValue expr && isCast expr && t1 == DynType && t2' == DynType && isGroundType t2 && t1' == t2 =
            evaluationStyle expr'
    -- FAIL - cast fails
    | isValue expr && isCast expr && t1 == DynType && t2' == DynType &&
        isGroundType t2 && isGroundType t1' && not (sameGround t1' t2) =
            Blame t2 $ "cannot cast from " ++ show t1' ++ " to " ++ show t2
    -- SUCCEEDFAIL/\ - cast is either sucessful or fails
    | isValue expr && isIntersectionCasts expr =
        evaluationStyle $ IntersectionCasts (map (\x -> SingleCast (getCastLabel x) t1 t2 x) cs) expr''
    -- GROUND - cast types through their ground types
    | isValue expr && not (isGroundType t1) && t2 == DynType =
        let g = getGroundType t1
        in evaluationStyle $ Cast g DynType $ Cast t1 g expr
    -- EXPAND - cast types through their ground types
    | isValue expr && not (isGroundType t2) && t1 == DynType =
        let g = getGroundType t2
        in evaluationStyle $ Cast g t2 $ Cast DynType g expr
    -- Project types and expression from inner casts
    where (Cast t1' t2' expr') = expr
          (IntersectionCasts cs expr'') = expr

-- if expression is an intersection of casts
evaluate e@(IntersectionCasts cs expr)
    -- push blame to top level
    | isBlame expr = evaluationStyle expr
    -- group all casts to be reduced
    | isIntersectionCasts expr || isCast expr = evaluationStyle $ mergeCasts e
    -- values don't reduce
    | isValue e = e
    -- evaluate inside a cast
    | not (isValue expr) =
        let expr2 = evaluate expr
        in evaluationStyle $ IntersectionCasts cs expr2
    -- Remove empty casts
    | isValue expr && all isEmptyCast cs = evaluationStyle expr
    -- Propagate blame
    | isValue expr && all isBlameCast cs =
        let (BlameCast _ t msg) = head cs
        in evaluationStyle $ Blame t msg
    -- reduce the casts in the intersection
    | otherwise =
        let
            -- reduce intersection casts and filter stuck ones
            cs' = filter (not . isStuckCast) $ map evaluateCasts cs
            result
                -- if there is only one intersection cast, convert to cast
                | length cs' == 1 = convertToSimpleCast (head cs') expr
                -- otherwise return intersection of casts
                | not $ null cs' = IntersectionCasts cs' expr
        in evaluationStyle result

-- blames are values
evaluate e@Blame{} = e

-- evaluate casts in intersections
evaluateCasts :: CastI -> CastI

-- evaluate single casts
evaluateCasts c@(SingleCast cl t1 t2 c1)
    -- push blames to top level
    | isBlameCast c1 =
        let (BlameCast _ _ msg) = c1
        in BlameCast cl t2 msg
    -- push stuck to top level
    | isStuckCast c1 = StuckCast cl t2
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
            BlameCast cl t2 $ "cannot cast from " ++ show t1' ++ " to " ++ show t2
    -- GROUND - cast types through their ground types
    | (isCastValue1 c1 || isEmptyCast c1) && not (isGroundType t1) && t2 == DynType =
        let g = getGroundType t1
        in evaluateCastStyle $ SingleCast cl g DynType $ SingleCast cl t1 g c1
    -- EXPAND - cast types through their ground types
    | (isCastValue1 c1 || isEmptyCast c1) && not (isGroundType t2) && t1 == DynType =
        let g = getGroundType t2
        in evaluateCastStyle $ SingleCast cl g t2 $ SingleCast cl DynType g c1
    | isCastValue1 c1 || isEmptyCast c1 = StuckCast cl t2
    -- Project types and expression from inner casts
    where (SingleCast cl' t1' t2' c') = c1

-- blame cast evaluates to itself
evaluateCasts c@BlameCast{} = c

-- empty cast evaluates to itself
evaluateCasts c@EmptyCast{} = c

-- stuck cast evaluates to itself
evaluateCasts c@StuckCast{} = c
