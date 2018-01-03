module Examples where

-- Syntax & Types
import Syntax
import Types

-- NON INTERSECTION TYPE EXAMPLES

-- (\\x : Dyn . x 1) (\\y : Int . y + 1) : Dyn
-- -->s (2 : Int => Dyn)
example0 = TypeInformation DynType (Application (TypeInformation (ArrowType DynType DynType) (Abstraction "x" DynType (TypeInformation DynType (Application (TypeInformation DynType (Variable "x")) (TypeInformation IntType (Int 1)))))) (TypeInformation (ArrowType IntType IntType) (Abstraction "y" IntType (TypeInformation IntType (Addition (TypeInformation IntType (Variable "y")) (TypeInformation IntType (Int 1)))))))

-- (\\x : Dyn . x true) (\\y : Int . y + 1)
-- -->s blame Bool => Int
example0fail = TypeInformation DynType (Application (TypeInformation (ArrowType DynType DynType) (Abstraction "x" DynType (TypeInformation DynType (Application (TypeInformation DynType (Variable "x")) (TypeInformation BoolType (Bool True)))))) (TypeInformation (ArrowType IntType IntType) (Abstraction "y" IntType (TypeInformation IntType (Addition (TypeInformation IntType (Variable "y")) (TypeInformation IntType (Int 1)))))))


-- INTERSECTION TYPE EXAMPLES

-- (\x : Int -> Int /\ Dyn . x 1 + x true) (\y : Int . y + 1) : Int
-- -->/\G blame Bool => Int
example3 = TypeInformation IntType (Application (TypeInformation (ArrowType (IntersectionType [ArrowType IntType IntType, DynType]) IntType) (Abstraction "x" (IntersectionType [ArrowType IntType IntType, DynType]) (TypeInformation IntType (Addition (TypeInformation IntType (Application (TypeInformation (ArrowType IntType IntType) (Variable "x")) (TypeInformation IntType (Int 1)))) (TypeInformation DynType (Application (TypeInformation DynType (Variable "x")) (TypeInformation BoolType (Bool True)))))))) (TypeInformation (ArrowType IntType IntType) (Abstraction "y" IntType (TypeInformation IntType (Addition (TypeInformation IntType (Variable "y")) (TypeInformation IntType (Int 1)))))))

-- (\x : Int /\ Dyn . x + x) 1 : Int
-- -->/\ 2
example4 = TypeInformation (IntType ) $ Application (TypeInformation (ArrowType (IntersectionType [IntType, DynType]) IntType) $ Abstraction "x" (IntersectionType [IntType, DynType]) $ TypeInformation IntType $ Addition (TypeInformation IntType $ Variable "x") (TypeInformation DynType $ Variable "x")) (TypeInformation IntType $ Int 1)

t' = ArrowType (IntersectionType [ArrowType (IntersectionType [IntType, DynType]) IntType, DynType]) IntType

-- (\x : (((Int /\ Dyn) -> Int) /\ Dyn) -> Int . x (\y : Int . y + 1)) (\z : Int -> Int . z 1) : Int
-- -->/\ 2
example5 = TypeInformation (IntType) $ Application (TypeInformation (ArrowType t' IntType) $ Abstraction "x" t' $ TypeInformation (IntType) $ Application (TypeInformation (t') $ Variable "x") (TypeInformation (ArrowType IntType IntType) $ Abstraction "y" IntType $ TypeInformation (IntType) $ Addition (TypeInformation (IntType) $ Variable "y") (TypeInformation (IntType) $ Int 1))) (TypeInformation (ArrowType (ArrowType IntType IntType) IntType) $ Abstraction "z" (ArrowType IntType IntType) $ TypeInformation IntType $ Application (TypeInformation (ArrowType IntType IntType) $ Variable "z") (TypeInformation (IntType) $ Int 1))

-- (\x : (((Int /\ Dyn) -> Int) /\ Dyn) -> Int . x (\y : Dyn . y + 1)) (\z : Dyn . z true) : Int
-- --> /\ blame Bool => Int
example5Dyn = TypeInformation (IntType) $ Application (TypeInformation (ArrowType t' IntType) $ Abstraction "x" t' $ TypeInformation (IntType) $ Application (TypeInformation (t') $ Variable "x") (TypeInformation (ArrowType DynType IntType) $ Abstraction "y" DynType $ TypeInformation (IntType) $ Addition (TypeInformation (DynType) $ Variable "y") (TypeInformation (IntType) $ Int 1))) (TypeInformation (ArrowType DynType DynType) $ Abstraction "z" DynType $ TypeInformation DynType $ Application (TypeInformation DynType $ Variable "z") (TypeInformation (BoolType) $ Bool True))

-- SELF APPLICATION EXAMPLES

t1 = IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType), ArrowType IntType IntType]
t2 = IntersectionType [ArrowType IntType IntType, IntType]

-- (\x : ((Int -> Int) -> Int -> Int) /\ (Int -> Int) . x x) (\y : Int -> Int /\ Int . y) : Int -> Int
-- -->/\ (\y : Int -> Int /\ Int . y) : Int -> Int
selfApplication = TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (t1) (ArrowType IntType IntType)) $ Abstraction "x" t1 $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType )) $ Variable "x") (TypeInformation (ArrowType IntType IntType ) $ Variable "x")) (TypeInformation (t1) $ Abstraction "y" t2 $ TypeInformation (t2) $ Variable "y")

-- (\x : ((Int -> Int) -> Int -> Int) /\ (Int -> Int) . x x) (\y : Int -> Int /\ Int . y) 1 : Int
-- -->/\ 1
selfApplicationApp = TypeInformation (IntType) $ Application (TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (t1) (ArrowType IntType IntType)) $ Abstraction "x" t1 $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType )) $ Variable "x") (TypeInformation (ArrowType IntType IntType ) $ Variable "x")) (TypeInformation (t1) $ Abstraction "y" t2 $ TypeInformation (t2) $ Variable "y")) (TypeInformation (IntType) $ Int 1)

-- (\x : Dyn . x x) (\y : Int -> Int /\ Int . y) : Dyn
-- -->/\ (\y : Int -> Int /\ Int . y) : ... /\ ... /\ ... /\ ...
selfApplication1Dyn = TypeInformation DynType $ Application (TypeInformation (ArrowType (DynType) DynType) $ Abstraction "x" DynType $ TypeInformation (DynType) $ Application (TypeInformation DynType $ Variable "x") (TypeInformation DynType $ Variable "x")) (TypeInformation (t1) $ Abstraction "y" t2 $ TypeInformation (t2) $ Variable "y")

-- (\x : Dyn . x x) (\y : Int -> Int /\ Int . y) 1 : Dyn
-- -->/\ 1 : (blame Int (Dyn -> Dyn)) /\ Int => Dyn
selfApplication1DynApp = TypeInformation (DynType) $ Application (TypeInformation DynType $ Application (TypeInformation (ArrowType (DynType) DynType) $ Abstraction "x" DynType $ TypeInformation (DynType) $ Application (TypeInformation DynType $ Variable "x") (TypeInformation DynType $ Variable "x")) (TypeInformation (t1) $ Abstraction "y" t2 $ TypeInformation (t2) $ Variable "y")) (TypeInformation IntType $ Int 1)

-- (\x : Dyn . x x) (\y : Int -> Int /\ Int . y) True : Dyn
-- -->/\ blame Bool => Int
selfApplication1DynAppfail = TypeInformation (DynType) $ Application (TypeInformation DynType $ Application (TypeInformation (ArrowType (DynType) DynType) $ Abstraction "x" DynType $ TypeInformation (DynType) $ Application (TypeInformation DynType $ Variable "x") (TypeInformation DynType $ Variable "x")) (TypeInformation (t1) $ Abstraction "y" t2 $ TypeInformation (t2) $ Variable "y")) (TypeInformation BoolType $ Bool True)

-- (\x : ((Int -> Int) -> Int -> Int) /\ (Int -> Int). x x) (\y : Dyn . y) 1 : Int
-- -->/\ 1
selfApplication2DynApp = TypeInformation (IntType) $ Application (TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (t1) (ArrowType IntType IntType)) $ Abstraction "x" t1 $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType)) $ Variable "x") (TypeInformation (ArrowType IntType IntType) $ Variable "x")) (TypeInformation (ArrowType DynType DynType) $ Abstraction "y" DynType $ TypeInformation (DynType) $ Variable "y")) (TypeInformation IntType $ Int 1)

-- (\\x : Dyn /\ Int -> Int . x x) (\\y : Int -> Int /\ Int . y) 1 : Dyn
-- -->/\ 1 : Int => Dyn
selfApplication11DynApp = TypeInformation (DynType) $ Application (TypeInformation DynType $ Application (TypeInformation (ArrowType (IntersectionType [DynType, ArrowType IntType IntType]) DynType) $ Abstraction "x" (IntersectionType [DynType, ArrowType IntType IntType]) $ TypeInformation (DynType) $ Application (TypeInformation DynType $ Variable "x") (TypeInformation (ArrowType IntType IntType) $ Variable "x")) (TypeInformation (t1) $ Abstraction "y" t2 $ TypeInformation (t2) $ Variable "y")) (TypeInformation IntType $ Int 1)

-- (\\x : (Int -> Int) -> Int -> Int /\ Dyn . x x) (\\y : Int -> Int /\ Int . y) 1 : Int
-- -->/\ 1
selfApplication12DynApp = TypeInformation (IntType) $ Application (TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType), DynType]) (ArrowType IntType IntType)) $ Abstraction "x" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType), DynType]) $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType)) $ Variable "x") (TypeInformation DynType $ Variable "x")) (TypeInformation (t1) $ Abstraction "y" t2 $ TypeInformation (t2) $ Variable "y")) (TypeInformation IntType $ Int 1)

-- (\x : ((Int -> Int) -> Int -> Int) /\ (Int -> Int) . x x) (\y : Dyn /\ Int . y) 1 : Int
-- -->/\ 1
selfApplication21DynApp = TypeInformation (IntType) $ Application (TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (t1) (ArrowType IntType IntType)) $ Abstraction "x" t1 $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType )) $ Variable "x") (TypeInformation (ArrowType IntType IntType ) $ Variable "x")) (TypeInformation (IntersectionType [ArrowType DynType DynType, ArrowType IntType IntType]) $ Abstraction "y" (IntersectionType [DynType, IntType]) $ TypeInformation (IntersectionType [DynType, IntType]) $ Variable "y")) (TypeInformation (IntType) $ Int 1)

-- (\x : ((Int -> Int) -> Int -> Int) /\ (Int -> Int) . x x) (\y : Int -> Int /\ Dyn . y) 1 : Int
-- -->/\ 1
selfApplication22DynApp = TypeInformation (IntType) $ Application (TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (t1) (ArrowType IntType IntType)) $ Abstraction "x" t1 $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType )) $ Variable "x") (TypeInformation (ArrowType IntType IntType ) $ Variable "x")) (TypeInformation (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType), ArrowType DynType DynType]) $ Abstraction "y" (IntersectionType [ArrowType IntType IntType, DynType]) $ TypeInformation (IntersectionType [ArrowType IntType IntType, DynType]) $ Variable "y")) (TypeInformation (IntType) $ Int 1)

-- (\x : Dyn /\ (Int -> Int) . x x) (\y : Dyn /\ Int . y) 1 : Dyn
-- -->/\ 1 : Int => Dyn
selfApplication11Dyn21DynApp = TypeInformation (DynType) $ Application (TypeInformation DynType $ Application (TypeInformation (ArrowType (IntersectionType [DynType, ArrowType IntType IntType]) DynType) $ Abstraction "x" (IntersectionType [DynType, ArrowType IntType IntType]) $ TypeInformation DynType $ Application (TypeInformation DynType $ Variable "x") (TypeInformation (ArrowType IntType IntType ) $ Variable "x")) (TypeInformation (IntersectionType [ArrowType DynType DynType, ArrowType IntType IntType]) $ Abstraction "y" (IntersectionType [DynType, IntType]) $ TypeInformation (IntersectionType [DynType, IntType]) $ Variable "y")) (TypeInformation (IntType) $ Int 1)
