module PrettyPrinter (
    prettyExpression,
    prettyType
) where

-- Syntax & Types
import Syntax
import Types

-- Imports
import Data.List
import Text.PrettyPrint.Leijen

-- pretty print expression
prettyExpression :: Expression -> Doc

-- Typed λ-calculus terms
prettyExpression (Variable var typ) = text var
prettyExpression (Abstraction var typ expr) = hcat
    [backslash, text var, space, colon, space, prettyType typ, space, dot,
        space, prettyExpression expr]
prettyExpression (Application expr1 expr2) = hsep
    [printParensExpression expr1, printParensExpression expr2]

-- Integers
prettyExpression (Int i) = int i

-- Booleans
prettyExpression (Bool b) = text $ show b

-- Arithmetic operators
prettyExpression (Addition expr1 expr2) = hsep
    [printParensExpression expr1, text "+", printParensExpression expr2]

-- Type annotations
prettyExpression (TypeInformation typ expr) = hsep
    [printParensExpression expr, colon, prettyType typ]

-- Cast Intersection
prettyExpression (CastIntersection cs expr) = hsep
    [printParensExpression expr, colon, parens $ hcat $ punctuate (space <> text "/\\" <> space) $ map printParensCasts cs]

-- Blames
prettyExpression (Blame typ msg) = hsep
    [text "Blame:", text msg]

-- pretty print cast intersection
prettyCasts :: Cast -> Doc

-- Single cast
prettyCasts (SingleCast cl t1 t2 c) = hsep
    [printParensCasts c, colon, printParensType t1, text "=>", printParensType t2, int cl]

-- Blame
prettyCasts (BlameCast cl i f msg) = hsep
    [text "blame", printParensType i, printParensType f, text msg, int cl]

-- Empty Cast
prettyCasts (EmptyCast cl t) = hsep [text "(/)", printParensType t, int cl]

-- pretty print type
prettyType :: Type -> Doc

-- Type variable: Var
prettyType (VarType var) = text var

-- Arrow type: Type -> Type
prettyType (ArrowType t1 t2) = hsep
    [printParensType t1, text "->", prettyType t2]

-- Integer type: Int
prettyType IntType = text "Int"

-- Boolean type: Bool
prettyType BoolType = text "Bool"

-- Dynamic type: ?
prettyType DynType = text "Dyn"

-- Intersection type: Type /\ Type
prettyType (IntersectionType ts) = hcat $
    punctuate (space <> text "/\\" <> space) $ map printParensType ts

printParensExpression :: Expression -> Doc
printParensExpression expr = if needsParensExpression expr
    then parens (prettyExpression expr)
    else prettyExpression expr

needsParensExpression :: Expression -> Bool
needsParensExpression expr =
    isAbstraction expr ||
    isApplication expr ||
    isAddition expr ||
    isTypeInformation expr ||
    isCastIntersection expr ||
    isBlame expr

printParensCasts :: Cast -> Doc
printParensCasts c = if needsParensCasts c
    then parens (prettyCasts c)
    else prettyCasts c

needsParensCasts :: Cast -> Bool
needsParensCasts c =
    isSingleCast c

printParensType :: Type -> Doc
printParensType typ = if needsParensType typ
    then parens (prettyType typ)
    else prettyType typ

needsParensType :: Type -> Bool
needsParensType typ =
    isArrowType typ ||
    isIntersectionType typ
