module Fun.Interpreter.Runtime

open Fun.Semantics
open Fun.Interpreter.Exceptions

// TODO apply reflection here

module Builtins = 
    
    let builtin_types = ["Bool"]

    let bool_true = 
        let church_true = Func(fun x -> Func(fun y -> x))
        UserType(church_true, "Bool")
    
    let bool_false = 
        let church_false = Func(fun x -> Func(fun y -> y))
        UserType(church_false, "Bool")

    let typeConstructor typename = Func(fun v ->
            UserType(v, typename)
        )

    let typeUnwrap typename = Func(function
            | UserType(v, t) when t = typename -> v
            | v when v.Typename = typename -> v
            | v -> typeFail typename (v.Typename)
        )

    let typeCheck typename = Func(function
            | v when v.Typename = typename -> bool_true
            | _ -> bool_false
        )

    let unit_unwrap = typeUnwrap "unit"
    let unit_check = typeCheck "unit"