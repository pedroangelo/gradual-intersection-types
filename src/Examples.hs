module Examples where

-- Syntax & Types
import Syntax
import Types

-- NON INTERSECTION TYPE EXAMPLES

-- (\\x : Dyn . x 1) (\\y : Int . y + 1) : Dyn
-- -->s (2 : Int => Dyn)

example0 = Application (AnnotatedAbstraction "x" DynType (Application (Variable "x" DynType) (Int 1))) (AnnotatedAbstraction "y" IntType (Addition (Variable "y" IntType) (Int 1)))

example0_typed = TypeInformation DynType (Application (TypeInformation (ArrowType DynType DynType) (AnnotatedAbstraction "x" DynType (TypeInformation DynType (Application (TypeInformation DynType (Variable "x" DynType)) (TypeInformation IntType (Int 1)))))) (TypeInformation (ArrowType IntType IntType) (AnnotatedAbstraction "y" IntType (TypeInformation IntType (Addition (TypeInformation IntType (Variable "y" IntType)) (TypeInformation IntType (Int 1)))))))

-- (\\x : Dyn . x true) (\\y : Int . y + 1)
-- -->s blame Bool => Int

example0fail = Application (AnnotatedAbstraction "x" DynType (Application (Variable "x" DynType) (Bool True))) (AnnotatedAbstraction "y" IntType (Addition (Variable "y" IntType) (Int 1)))

example0fail_typed = TypeInformation DynType (Application (TypeInformation (ArrowType DynType DynType) (AnnotatedAbstraction "x" DynType (TypeInformation DynType (Application (TypeInformation DynType (Variable "x" DynType)) (TypeInformation BoolType (Bool True)))))) (TypeInformation (ArrowType IntType IntType) (AnnotatedAbstraction "y" IntType (TypeInformation IntType (Addition (TypeInformation IntType (Variable "y" IntType)) (TypeInformation IntType (Int 1)))))))

-- INTERSECTION TYPE EXAMPLES

-- (\x : Int -> Int /\ Dyn . x 1 + x 2) (\y : Int . y + 1) : Int
-- -->/\G 5
-- Instantiating casts according to type of variable yields same result

example3 = Application (AnnotatedAbstraction "x" (IntersectionType [ArrowType IntType IntType,DynType]) (Addition (Application (Variable "x" (ArrowType IntType IntType)) (Int 1)) (Application (Variable "x" DynType) (Int 2)))) (AnnotatedAbstraction "y" IntType (Addition (Variable "y" IntType) (Int 1)))

example3_typed = TypeInformation IntType (Application (TypeInformation (ArrowType (IntersectionType [ArrowType IntType IntType, DynType]) IntType) (AnnotatedAbstraction "x" (IntersectionType [ArrowType IntType IntType, DynType]) (TypeInformation IntType (Addition (TypeInformation IntType (Application (TypeInformation (ArrowType IntType IntType) (Variable "x" $ ArrowType IntType IntType)) (TypeInformation IntType (Int 1)))) (TypeInformation DynType (Application (TypeInformation DynType (Variable "x" DynType)) (TypeInformation IntType (Int 2)))))))) (TypeInformation (ArrowType IntType IntType) (AnnotatedAbstraction "y" IntType (TypeInformation IntType (Addition (TypeInformation IntType (Variable "y" IntType)) (TypeInformation IntType (Int 1)))))))

example3_typed2 = TypeInformation IntType (Application (TypeInformation (ArrowType (IntersectionType [ArrowType IntType IntType,DynType]) IntType) (AnnotatedAbstraction "x" (IntersectionType [ArrowType IntType IntType,DynType]) (TypeInformation IntType (Addition (TypeInformation IntType (Application (TypeInformation (ArrowType IntType IntType) (Variable "x" (ArrowType IntType IntType))) (TypeInformation IntType (Int 1)))) (TypeInformation DynType (Application (TypeInformation DynType (Variable "x" DynType)) (TypeInformation IntType (Int 2)))))))) (TypeInformation (IntersectionType [ArrowType IntType IntType,ArrowType IntType IntType]) (AnnotatedAbstraction "y" IntType (TypeInformation IntType (Addition (TypeInformation (IntersectionType [IntType,IntType]) (Variable "y" (IntersectionType [IntType,IntType]))) (TypeInformation IntType (Int 1)))))))

-- (\x : Int -> Int /\ Dyn . x 1 + x true) (\y : Int . y + 1) : Int
-- -->/\G blame Bool => Int
-- Instantiating casts according to type of variable yields same result

example3fail = Application (AnnotatedAbstraction "x" (IntersectionType [ArrowType IntType IntType,DynType]) (Addition (Application (Variable "x" (ArrowType IntType IntType)) (Int 1)) (Application (Variable "x" DynType) (Bool True)))) (AnnotatedAbstraction "y" IntType (Addition (Variable "y" IntType) (Int 1)))

example3fail_typed = TypeInformation IntType (Application (TypeInformation (ArrowType (IntersectionType [ArrowType IntType IntType, DynType]) IntType) (AnnotatedAbstraction "x" (IntersectionType [ArrowType IntType IntType, DynType]) (TypeInformation IntType (Addition (TypeInformation IntType (Application (TypeInformation (ArrowType IntType IntType) (Variable "x" $ ArrowType IntType IntType)) (TypeInformation IntType (Int 1)))) (TypeInformation DynType (Application (TypeInformation DynType (Variable "x" DynType)) (TypeInformation BoolType (Bool True)))))))) (TypeInformation (ArrowType IntType IntType) (AnnotatedAbstraction "y" IntType (TypeInformation IntType (Addition (TypeInformation IntType (Variable "y" IntType)) (TypeInformation IntType (Int 1)))))))

example3fail_typed2 = TypeInformation IntType (Application (TypeInformation (ArrowType (IntersectionType [ArrowType IntType IntType,DynType]) IntType) (AnnotatedAbstraction "x" (IntersectionType [ArrowType IntType IntType,DynType]) (TypeInformation IntType (Addition (TypeInformation IntType (Application (TypeInformation (ArrowType IntType IntType) (Variable "x" (ArrowType IntType IntType))) (TypeInformation IntType (Int 1)))) (TypeInformation DynType (Application (TypeInformation DynType (Variable "x" DynType)) (TypeInformation BoolType (Bool True)))))))) (TypeInformation (IntersectionType [ArrowType IntType IntType,ArrowType IntType IntType]) (AnnotatedAbstraction "y" IntType (TypeInformation IntType (Addition (TypeInformation (IntersectionType [IntType,IntType]) (Variable "y" (IntersectionType [IntType,IntType]))) (TypeInformation IntType (Int 1)))))))

-- (\x : Int /\ Dyn . x + x) 1 : Int
-- -->/\ 2
-- Instantiating casts according to type of variable yields same result

example4 = Application (AnnotatedAbstraction "x" (IntersectionType [IntType,DynType]) (Addition (Variable "x" IntType) (Variable "x" DynType))) (Int 1)

example4_typed = TypeInformation (IntType ) $ Application (TypeInformation (ArrowType (IntersectionType [IntType, DynType]) IntType) $ AnnotatedAbstraction "x" (IntersectionType [IntType, DynType]) $ TypeInformation IntType $ Addition (TypeInformation IntType $ Variable "x" IntType) (TypeInformation DynType $ Variable "x" DynType)) (TypeInformation IntType $ Int 1)

-- (\x : Int /\ Dyn . x) 1 : Dyn
-- -->/\ 1 : ((/) Int 0 /\ ((/) Int 0 : Int => Dyn 0))
-- Instantiating casts according to type of variable yields the result: 1

-- not possible under type inference
example6 = TypeInformation (DynType) $ Application (TypeInformation (ArrowType (IntersectionType [IntType, DynType]) DynType) $ AnnotatedAbstraction "x" (IntersectionType [IntType, DynType]) $ (TypeInformation DynType $ Variable "x" DynType)) (TypeInformation IntType $ Int 1)

t' = ArrowType (IntersectionType [ArrowType (IntersectionType [IntType, DynType]) IntType, DynType]) IntType

-- (\x : (((Int /\ Dyn) -> Int) /\ Dyn) -> Int . x (\y : Int . y + 1)) (\z : Int -> Int . z 1) : Int
-- -->/\ 2
-- Instantiating casts according to type of variable yields same result

-- not possible under type inference
example5 = TypeInformation (IntType) $ Application (TypeInformation (ArrowType t' IntType) $ AnnotatedAbstraction "x" t' $ TypeInformation (IntType) $ Application (TypeInformation (t') $ Variable "x" t') (TypeInformation (ArrowType IntType IntType) $ AnnotatedAbstraction "y" IntType $ TypeInformation (IntType) $ Addition (TypeInformation (IntType) $ Variable "y" IntType) (TypeInformation (IntType) $ Int 1))) (TypeInformation (ArrowType (ArrowType IntType IntType) IntType) $ AnnotatedAbstraction "z" (ArrowType IntType IntType) $ TypeInformation IntType $ Application (TypeInformation (ArrowType IntType IntType) $ Variable "z" $ArrowType IntType IntType) (TypeInformation (IntType) $ Int 1))

-- (\x : (((Int /\ Dyn) -> Int) /\ Dyn) -> Int . x (\y : Dyn . y + 1)) (\z : Dyn . z true) : Int
-- -->/\ blame Bool => Int
example5Dyn = TypeInformation (IntType) $ Application (TypeInformation (ArrowType t' IntType) $ AnnotatedAbstraction "x" t' $ TypeInformation (IntType) $ Application (TypeInformation (t') $ Variable "x" t') (TypeInformation (ArrowType DynType IntType) $ AnnotatedAbstraction "y" DynType $ TypeInformation (IntType) $ Addition (TypeInformation (DynType) $ Variable "y" DynType) (TypeInformation (IntType) $ Int 1))) (TypeInformation (ArrowType DynType DynType) $ AnnotatedAbstraction "z" DynType $ TypeInformation DynType $ Application (TypeInformation DynType $ Variable "z" DynType) (TypeInformation (BoolType) $ Bool True))

-- (\y : Int -> Int /\ Int . (\x . x x) y)

example7 = AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType, IntType]) $ Application (Abstraction "x" $ Application (Variable "x" $ ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType)) (Variable "x" $ ArrowType IntType IntType))  (Variable "y" $ IntersectionType [ArrowType IntType IntType, IntType]) 

example7_typed = TypeInformation (ArrowType (IntersectionType [ArrowType IntType IntType,IntType]) IntType) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,IntType]) (TypeInformation IntType (Application (TypeInformation (ArrowType (IntersectionType [ArrowType IntType IntType,IntType]) IntType) (Abstraction "x" (TypeInformation IntType (Application (TypeInformation (ArrowType IntType IntType) (Variable "x" (ArrowType IntType IntType))) (TypeInformation IntType (Variable "x" IntType)))))) (TypeInformation (IntersectionType [ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,IntType]))))))


-- SELF APPLICATION EXAMPLES

t1 = IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType), ArrowType IntType IntType]
t2 = IntersectionType [ArrowType IntType IntType, IntType]

-- (\x : ((Int -> Int) -> Int -> Int) /\ (Int -> Int) . x x) (\y : Int -> Int /\ Int . y) : Int -> Int
-- -->/\ (\y : Int -> Int /\ Int . y) : Int -> Int

selfApplication = Application (AnnotatedAbstraction "x" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType]) (Application (Variable "x" (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType))) (Variable "x" (ArrowType IntType IntType)))) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,IntType])))

selfApplication_typed = TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (t1) (ArrowType IntType IntType)) $ AnnotatedAbstraction "x" t1 $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType )) $ Variable "x" $ (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType ))) (TypeInformation (ArrowType IntType IntType ) $ Variable "x" $ ArrowType IntType IntType)) (TypeInformation (t1) $ AnnotatedAbstraction "y" t2 $ TypeInformation (t2) $ Variable "y" t2)

-- (\x : ((Int -> Int) -> Int -> Int) /\ (Int -> Int) . x x) (\y : Int -> Int /\ Int . y) 1 : Int
-- -->/\ 1

selfApplicationApp = Application (Application (AnnotatedAbstraction "x" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType]) (Application (Variable "x" (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType))) (Variable "x" (ArrowType IntType IntType)))) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,IntType])))) (Int 1)

selfApplicationApp_typed = TypeInformation (IntType) $ Application (TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (t1) (ArrowType IntType IntType)) $ AnnotatedAbstraction "x" t1 $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType )) $ Variable "x" $ (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType ))) (TypeInformation (ArrowType IntType IntType ) $ Variable "x" $ ArrowType IntType IntType)) (TypeInformation (t1) $ AnnotatedAbstraction "y" t2 $ TypeInformation (t2) $ Variable "y" $ t2)) (TypeInformation (IntType) $ Int 1)

-- (\x : Dyn . x x) (\y : Int -> Int /\ Int . y) : Dyn
-- -->/\ (\y : Int -> Int /\ Int . y) : ... /\ ... /\ ... /\ ... : Dyn
-- Instantiating casts according to type of variable yields same result

selfApplication1Dyn = Application (AnnotatedAbstraction "x" DynType (Application (Variable "x" DynType) (Variable "x" DynType))) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,IntType])))

selfApplication1Dyn_typed = TypeInformation DynType $ Application (TypeInformation (ArrowType (DynType) DynType) $ AnnotatedAbstraction "x" DynType $ TypeInformation (DynType) $ Application (TypeInformation DynType $ Variable "x" DynType) (TypeInformation DynType $ Variable "x" DynType)) (TypeInformation (t1) $ AnnotatedAbstraction "y" t2 $ TypeInformation (t2) $ Variable "y" t2)

selfApplication1Dyn_typed2 = TypeInformation DynType (Application (TypeInformation (ArrowType (IntersectionType [DynType,DynType]) DynType) (AnnotatedAbstraction "x" DynType (TypeInformation DynType (Application (TypeInformation DynType (Variable "x" DynType)) (TypeInformation DynType (Variable "x" DynType)))))) (TypeInformation (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType]) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,IntType]) (TypeInformation (IntersectionType [ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,IntType]))))))

-- (\x : Dyn . x x) (\y : Int -> Int /\ Int . y) 1 : Dyn
-- -->/\ 1 : (blame Int Dyn) /\ Int => Dyn : Dyn
-- Instantiating casts according to type of variable yields same result

selfApplication1DynApp = Application (Application (AnnotatedAbstraction "x" DynType (Application (Variable "x" DynType) (Variable "x" DynType))) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,IntType])))) (Int 1)

selfApplication1DynApp_typed = TypeInformation (DynType) $ Application (TypeInformation DynType $ Application (TypeInformation (ArrowType (DynType) DynType) $ AnnotatedAbstraction "x" DynType $ TypeInformation (DynType) $ Application (TypeInformation DynType $ Variable "x" DynType) (TypeInformation DynType $ Variable "x" DynType)) (TypeInformation (t1) $ AnnotatedAbstraction "y" t2 $ TypeInformation (t2) $ Variable "y" t2)) (TypeInformation IntType $ Int 1)

selfApplication1DynApp_typed2 = TypeInformation DynType (Application (TypeInformation DynType (Application (TypeInformation (ArrowType (IntersectionType [DynType,DynType]) DynType) (AnnotatedAbstraction "x" DynType (TypeInformation DynType (Application (TypeInformation DynType (Variable "x" DynType)) (TypeInformation DynType (Variable "x" DynType)))))) (TypeInformation (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType]) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,IntType]) (TypeInformation (IntersectionType [ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,IntType]))))))) (TypeInformation IntType (Int 1)))

-- (\x : Dyn . x x) (\y : Int -> Int /\ Int . y) True : Dyn
-- -->/\ blame (Dyn) Bool => Int : Dyn
-- Instantiating casts according to type of variable yields same result

selfApplication1DynAppfail = Application (Application (AnnotatedAbstraction "x" DynType (Application (Variable "x" DynType) (Variable "x" DynType))) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,IntType])))) (Bool True)

selfApplication1DynAppfail_typed = TypeInformation (DynType) $ Application (TypeInformation DynType $ Application (TypeInformation (ArrowType (DynType) DynType) $ AnnotatedAbstraction "x" DynType $ TypeInformation (DynType) $ Application (TypeInformation DynType $ Variable "x" DynType) (TypeInformation DynType $ Variable "x" DynType)) (TypeInformation (t1) $ AnnotatedAbstraction "y" t2 $ TypeInformation (t2) $ Variable "y" t2)) (TypeInformation BoolType $ Bool True)

selfApplication1DynAppfail_typed2 = TypeInformation DynType (Application (TypeInformation DynType (Application (TypeInformation (ArrowType (IntersectionType [DynType,DynType]) DynType) (AnnotatedAbstraction "x" DynType (TypeInformation DynType (Application (TypeInformation DynType (Variable "x" DynType)) (TypeInformation DynType (Variable "x" DynType)))))) (TypeInformation (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType]) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,IntType]) (TypeInformation (IntersectionType [ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,IntType]))))))) (TypeInformation BoolType (Bool True)))

-- (\x : ((Int -> Int) -> Int -> Int) /\ (Int -> Int). x x) (\y : Dyn . y) 1 : Int
-- -->/\ 1
-- Instantiating casts according to type of variable yields same result

selfApplication2DynApp = Application (Application (AnnotatedAbstraction "x" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType]) (Application (Variable "x" (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType))) (Variable "x" (ArrowType IntType IntType)))) (AnnotatedAbstraction "y" DynType (Variable "y" DynType))) (Int 1)

selfApplication2DynApp_typed = TypeInformation (IntType) $ Application (TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (t1) (ArrowType IntType IntType)) $ AnnotatedAbstraction "x" t1 $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType)) $ Variable "x" (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType ))) (TypeInformation (ArrowType IntType IntType) $ Variable "x" $ ArrowType IntType IntType)) (TypeInformation (ArrowType DynType DynType) $ AnnotatedAbstraction "y" DynType $ TypeInformation (DynType) $ Variable "y" DynType)) (TypeInformation IntType $ Int 1)

selfApplication2DynApp_typed2 = TypeInformation IntType (Application (TypeInformation (ArrowType IntType IntType) (Application (TypeInformation (ArrowType (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType]) (ArrowType IntType IntType)) (AnnotatedAbstraction "x" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType]) (TypeInformation (ArrowType IntType IntType) (Application (TypeInformation (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType)) (Variable "x" (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType)))) (TypeInformation (ArrowType IntType IntType) (Variable "x" (ArrowType IntType IntType))))))) (TypeInformation (IntersectionType [ArrowType DynType DynType,ArrowType DynType DynType]) (AnnotatedAbstraction "y" DynType (TypeInformation (IntersectionType [DynType,DynType]) (Variable "y" (IntersectionType [DynType,DynType]))))))) (TypeInformation IntType (Int 1)))

-- (\\x : Dyn /\ Int -> Int . x x) (\\y : Int -> Int /\ Int . y) 1 : Dyn
-- -->/\ 1 : Int => Dyn
-- Instantiating casts according to type of variable yields same result

selfApplication11DynApp = Application (Application (AnnotatedAbstraction "x" (IntersectionType [DynType,ArrowType IntType IntType]) (Application (Variable "x" DynType) (Variable "x" (ArrowType IntType IntType)))) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,IntType])))) (Int 1)

selfApplication11DynApp_typed = TypeInformation (DynType) $ Application (TypeInformation DynType $ Application (TypeInformation (ArrowType (IntersectionType [DynType, ArrowType IntType IntType]) DynType) $ AnnotatedAbstraction "x" (IntersectionType [DynType, ArrowType IntType IntType]) $ TypeInformation (DynType) $ Application (TypeInformation DynType $ Variable "x" DynType) (TypeInformation (ArrowType IntType IntType) $ Variable "x" $ ArrowType IntType IntType)) (TypeInformation (t1) $ AnnotatedAbstraction "y" t2 $ TypeInformation (t2) $ Variable "y" t2)) (TypeInformation IntType $ Int 1)

-- (\\x : (Int -> Int) -> Int -> Int /\ Dyn . x x) (\\y : Int -> Int /\ Int . y) 1 : Int
-- -->/\ 1
-- Instantiating casts according to type of variable yields same result

selfApplication12DynApp = Application (Application (AnnotatedAbstraction "x" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),DynType]) (Application (Variable "x" (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType))) (Variable "x" DynType))) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,IntType])))) (Int 1)

selfApplication12DynApp_typed = TypeInformation (IntType) $ Application (TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType), DynType]) (ArrowType IntType IntType)) $ AnnotatedAbstraction "x" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType), DynType]) $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType)) $ Variable "x" (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType ))) (TypeInformation DynType $ Variable "x" DynType)) (TypeInformation (t1) $ AnnotatedAbstraction "y" t2 $ TypeInformation (t2) $ Variable "y" t2)) (TypeInformation IntType $ Int 1)

-- (\x : ((Int -> Int) -> Int -> Int) /\ (Int -> Int) . x x) (\y : Dyn /\ Int . y) 1 : Int
-- -->/\ 1
-- Instantiating casts according to type of variable yields same result

selfApplication21DynApp = Application (Application (AnnotatedAbstraction "x" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType]) (Application (Variable "x" (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType))) (Variable "x" (ArrowType IntType IntType)))) (AnnotatedAbstraction "y" (IntersectionType [DynType,IntType]) (Variable "y" (IntersectionType [DynType,IntType])))) (Int 1)

selfApplication21DynApp_typed = TypeInformation (IntType) $ Application (TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (t1) (ArrowType IntType IntType)) $ AnnotatedAbstraction "x" t1 $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType )) $ Variable "x" (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType ))) (TypeInformation (ArrowType IntType IntType ) $ Variable "x" $ ArrowType IntType IntType)) (TypeInformation (IntersectionType [ArrowType DynType DynType, ArrowType IntType IntType]) $ AnnotatedAbstraction "y" (IntersectionType [DynType, IntType]) $ TypeInformation (IntersectionType [DynType, IntType]) $ Variable "y" $ IntersectionType [DynType, IntType])) (TypeInformation (IntType) $ Int 1)

-- (\x : ((Int -> Int) -> Int -> Int) /\ (Int -> Int) . x x) (\y : Int -> Int /\ Dyn . y) 1 : Int
-- -->/\ 1
-- Instantiating casts according to type of variable yields same result

selfApplication22DynApp = Application (Application (AnnotatedAbstraction "x" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType]) (Application (Variable "x" (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType))) (Variable "x" (ArrowType IntType IntType)))) (AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType,DynType]) (Variable "y" (IntersectionType [ArrowType IntType IntType,DynType])))) (Int 1)

selfApplication22DynApp_typed = TypeInformation (IntType) $ Application (TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (t1) (ArrowType IntType IntType)) $ AnnotatedAbstraction "x" t1 $ TypeInformation (ArrowType IntType IntType) $ Application (TypeInformation (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType )) $ Variable "x" (ArrowType (ArrowType IntType IntType ) (ArrowType IntType IntType ))) (TypeInformation (ArrowType IntType IntType ) $ Variable "x" $ ArrowType IntType IntType)) (TypeInformation (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType), ArrowType DynType DynType]) $ AnnotatedAbstraction "y" (IntersectionType [ArrowType IntType IntType, DynType]) $ TypeInformation (IntersectionType [ArrowType IntType IntType, DynType]) $ Variable "y" $ IntersectionType [ArrowType IntType IntType, DynType])) (TypeInformation (IntType) $ Int 1)

-- (\x : Dyn /\ (Int -> Int) . x x) (\y : Dyn /\ Int . y) 1 : Dyn
-- -->/\ 1 : Int => Dyn
-- Instantiating casts according to type of variable yields same result

selfApplication11Dyn21DynApp = Application (Application (AnnotatedAbstraction "x" (IntersectionType [DynType,ArrowType IntType IntType]) (Application (Variable "x" DynType) (Variable "x" (ArrowType IntType IntType)))) (AnnotatedAbstraction "y" (IntersectionType [DynType,IntType]) (Variable "y" (IntersectionType [DynType,IntType])))) (Int 1)

selfApplication11Dyn21DynApp_typed = TypeInformation (DynType) $ Application (TypeInformation DynType $ Application (TypeInformation (ArrowType (IntersectionType [DynType, ArrowType IntType IntType]) DynType) $ AnnotatedAbstraction "x" (IntersectionType [DynType, ArrowType IntType IntType]) $ TypeInformation DynType $ Application (TypeInformation DynType $ Variable "x" DynType) (TypeInformation (ArrowType IntType IntType ) $ Variable "x" $ ArrowType IntType IntType)) (TypeInformation (IntersectionType [ArrowType DynType DynType, ArrowType IntType IntType]) $ AnnotatedAbstraction "y" (IntersectionType [DynType, IntType]) $ TypeInformation (IntersectionType [DynType, IntType]) $ Variable "y" $ IntersectionType [DynType, IntType])) (TypeInformation (IntType) $ Int 1)

-- (\x : Dyn /\ Int -> Int . x x x) (\y : (Int -> Int) -> Int -> Int /\ Int -> Int /\ Int . y) : Dyn
-- -->/\

tripleApplication11DynApp = Application (AnnotatedAbstraction "x" (IntersectionType [DynType,ArrowType IntType IntType]) (Application (Application (Variable "x" DynType) (Variable "x" DynType)) (Variable "x" (ArrowType IntType IntType)))) (AnnotatedAbstraction "y" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType), ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType), ArrowType IntType IntType,IntType])))

tripleApplication11DynApp_typed = TypeInformation DynType (Application (TypeInformation (ArrowType (IntersectionType [DynType,DynType,ArrowType IntType IntType]) DynType) (AnnotatedAbstraction "x" (IntersectionType [DynType,ArrowType IntType IntType]) (TypeInformation DynType (Application (TypeInformation DynType (Application (TypeInformation DynType (Variable "x" DynType)) (TypeInformation DynType (Variable "x" DynType)))) (TypeInformation (ArrowType IntType IntType) (Variable "x" (ArrowType IntType IntType))))))) (TypeInformation (IntersectionType [ArrowType (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType)) (ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType)),ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType]) (AnnotatedAbstraction "y" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType,IntType]) (TypeInformation (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType,IntType]) (Variable "y" (IntersectionType [ArrowType (ArrowType IntType IntType) (ArrowType IntType IntType),ArrowType IntType IntType,IntType]))))))

-- (\x : Dyn /\ Int . x x x) : Dyn
-- -->/\

triple1Dyn = AnnotatedAbstraction "x" (IntersectionType [DynType, IntType]) (Application (Application (Variable "x" DynType) (Variable "x" DynType)) (Variable "x" (DynType)))

triple1Dyn_typed = TypeInformation (ArrowType (IntersectionType [DynType,IntType,IntType]) DynType) (AnnotatedAbstraction "x" (IntersectionType [DynType,IntType]) (TypeInformation DynType (Application (TypeInformation DynType (Application (TypeInformation DynType (Variable "x" DynType)) (TypeInformation IntType (Variable "x" IntType)))) (TypeInformation IntType (Variable "x" IntType)))))

-- (\x : Dyn /\ Bool . x + x) : TypeError

example8 = AnnotatedAbstraction "x" (IntersectionType [DynType,BoolType]) (Addition (Variable "x" IntType) (Variable "x" IntType))

example9 = Application (Application (Abstraction "x" (Application (Variable "x" IntType) (Variable "x" IntType))) (AnnotatedAbstraction "y" DynType (Variable "y" IntType))) (Int 1)
