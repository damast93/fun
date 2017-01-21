module Fun.Interpreter.Runtime

open System
open Fun.Semantics
open Fun.Interpreter.Exceptions

// TODO apply reflection here

type internal Builtin(fn : string) = 
    inherit Attribute()
    member this.Name = fn

module Builtins = 
    
    let builtin_types = ["Bool"]

    [<Builtin("true")>]
    let bool_true = 
        let church_true = Func(fun x -> Func(fun y -> x))
        UserType(church_true, "Bool")
    
    [<Builtin("false")>]
    let bool_false = 
        let church_false = Func(fun x -> Func(fun y -> y))
        UserType(church_false, "Bool")

    let church b = if b then bool_true else bool_false

    let typeConstructor typename = Func(fun v ->
            UserType(v, typename)
        )

    let typeUnwrap typename = Func(function
            | UserType(v, t) when t = typename -> v
            | v when v.Typename = typename -> v
            | v -> typeFail typename (v.Typename)
        )

    let typeCheck typename = Func(fun v -> church (typename = v.Typename))
        
    [<Builtin("unit!")>]
    let unit_unwrap = typeUnwrap "unit"
    
    [<Builtin("unit?")>]
    let unit_check = typeCheck "unit"

    [<Builtin("int!")>]
    let int_unwrap = typeUnwrap "int"
    
    [<Builtin("int?")>]
    let int_check = typeCheck "int"    
    
    [<Builtin("float!")>]
    let float_unwrap = typeUnwrap "float"
    
    [<Builtin("float?")>]
    let float_check = typeCheck "float"
    
    [<Builtin("function!")>]
    let function_unwrap = typeUnwrap "function"
    
    [<Builtin("function?")>]
    let function_check = typeCheck "function"
    
    [<Builtin("array!")>]
    let array_unwrap = typeUnwrap "array"
    
    [<Builtin("array?")>]
    let array_check = typeCheck "array"
    
    [<Builtin("print")>]
    let print = Func(fun v ->
        printf "%s" (v.ToString())
        Unit
    )
    
    [<Builtin("printn")>]
    let printn = Func(fun v ->
        printfn "%s" (v.ToString())
        Unit
    )

    [<Builtin("+")>]
    let add = Func(fun a ->
        Func(fun b ->
            match a, b with
            | Int(x), Int(y) -> Int(x+y)
            | Float(x), Float(y) -> Float(x+y)
            | Int(x), _ -> typeFail "int" (b.Typename)
            | Float(x), _ -> typeFail "float" (b.Typename)
            | _ -> typeFail "int" (a.Typename)
        )
    )
    
    [<Builtin("-")>]
    let subtr = Func(fun a ->
        Func(fun b ->
            match a, b with
            | Int(x), Int(y) -> Int(x-y)
            | Float(x), Float(y) -> Float(x-y)
            | Int(x), _ -> typeFail "int" (b.Typename)
            | Float(x), _ -> typeFail "float" (b.Typename)
            | _ -> typeFail "int" (a.Typename)
        )
    )

    [<Builtin("*")>]
    let times = Func(fun a ->
        Func(fun b ->
            match a, b with
            | Int(x), Int(y) -> Int(x*y)
            | Float(x), Float(y) -> Float(x*y)
            | Int(x), _ -> typeFail "int" (b.Typename)
            | Float(x), _ -> typeFail "float" (b.Typename)
            | _ -> typeFail "int" (a.Typename)
        )
    )

    [<Builtin("/")>]
    let div = Func(fun a ->
        Func(fun b ->
            match a, b with
            | Int(x), Int(y) -> Int(x/y)
            | Float(x), Float(y) -> Float(x/y)
            | Int(x), _ -> typeFail "int" (b.Typename)
            | Float(x), _ -> typeFail "float" (b.Typename)
            | _ -> typeFail "int" (a.Typename)
        )
    )
    
    [<Builtin("<=")>]
    let leq = Func(fun a ->
        Func(fun b ->
            match a, b with
            | Int(x), Int(y) -> church (x <= y)
            | Float(x), Float(y) -> church (x <= y)
            | Int(x), _ -> typeFail "int" (b.Typename)
            | Float(x), _ -> typeFail "float" (b.Typename)
            | _ -> typeFail "int" (a.Typename)
        )
    )