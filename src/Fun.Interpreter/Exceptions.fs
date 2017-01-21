module internal Fun.Interpreter.Exceptions

open Fun.Semantics

let typeFail expected actual = failwithf "Type error: Expected `%s`, given `%s`" expected actual
let notInScopeFail name = failwithf "Not in scope: `%s`" name

let ofType typename (value : Value) = 
    match value with
    | v when v.Typename = typename -> v
    | v -> typeFail typename (v.Typename)
    
