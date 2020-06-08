# gradual-intersection-types
A gradually typed language with intersection types

### Instalation
Install stack ([Haskell Tool Stack](https://www.haskellstack.org/))
```
$ wget -qO- https://get.haskellstack.org/ | sh
```
Clone repository
```
$ git clone https://github.com/pedroangelo/gradual-intersection-types.git
```
Setup stack
```
$ cd gradual-intersection-types/
$ stack setup
$ stack ghci
```
### Summary of program phases and example
Infer type of an expression
```
*> :t inferType 
inferType :: Expression -> Either String Type
*> inferType example0
Right DynType
```
Annotate each sub-term of the expression with types
```
*> :t insertTypeInformation 
insertTypeInformation :: Expression -> Either String Expression
*> let (Right typed) = insertTypeInformation example0
```
Insert casts into expression
```
*> :t insertCasts 
insertCasts :: Expression -> Expression
*> let casted = insertCasts typed
```
Remove type annotations from expression
```
*> :t removeTypeInformation
removeTypeInformation :: Expression -> Expression
*> let casted' = removeTypeInformation casted
```
Evaluate expression
```
*> :t evaluate
evaluate :: Expression -> Expression
*> let result = evaluate casted'
```
Pretty printing
```
*> :t prettyExpression 
prettyExpression :: Expression -> Text.PrettyPrint.Leijen.Doc
*>:t prettyType 
prettyType :: Type -> Text.PrettyPrint.Leijen.Doc
*> prettyExpression result 
2 : (((/) Int 1 : Int => Dyn 1))
```
