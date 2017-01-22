module Fun.Interpreter.Runtime

open System
open System.Reflection

open Fun.Semantics
open Fun.Interpreter.Exceptions

type internal Builtin(fn : string) = 
    inherit Attribute()
    member this.Name = fn

type Builtins() = 
    static member BuiltinTypes = ["Bool"]

    static member GetBuiltins() = 
        let props =
            typeof<Builtins>.GetProperties()
            |> Array.choose (fun pi ->
                pi.CustomAttributes
                |> Seq.tryFind (fun attr -> attr.AttributeType = typeof<Builtin>)
                |> Option.map (fun attr -> pi, attr))
        
        props
        |> Seq.map (fun (pi, attr) ->
            let fn = unbox<string> attr.ConstructorArguments.[0].Value
            let value = pi.GetValue(null) :?> Value
            fn, value)
        |> Map.ofSeq

    [<Builtin("true")>]
    static member bool_true = 
        let church_true = Func(fun x -> Func(fun y -> x))
        UserType(church_true, "Bool")
    
    [<Builtin("false")>]
    static member bool_false = 
        let church_false = Func(fun x -> Func(fun y -> y))
        UserType(church_false, "Bool")

    static member church b = if b then Builtins.bool_true else Builtins.bool_false

    static member typeConstructor typename = Func(fun v ->
            UserType(v, typename)
        )

    static member typeUnwrap typename = Func(function
            | UserType(v, t) when t = typename -> v
            | v when v.Typename = typename -> v
            | v -> typeFail typename (v.Typename)
        )

    static member typeCheck t = Func(fun v -> Builtins.church (v.Typename = t))
        
    [<Builtin("unit!")>]
    static member unit_unwrap = Builtins.typeUnwrap "unit"
    
    [<Builtin("unit?")>]
    static member unit_check = Builtins.typeCheck "unit"

    [<Builtin("int!")>]
    static member int_unwrap = Builtins.typeUnwrap "int"
    
    [<Builtin("int?")>]
    static member int_check = Builtins.typeCheck "int"    
    
    [<Builtin("float!")>]
    static member float_unwrap = Builtins.typeUnwrap "float"
    
    [<Builtin("float?")>]
    static member float_check = Builtins.typeCheck "float"
    
    [<Builtin("function!")>]
    static member function_unwrap = Builtins.typeUnwrap "function"
    
    [<Builtin("function?")>]
    static member function_check = Builtins.typeCheck "function"
    
    [<Builtin("array!")>]
    static member array_unwrap = Builtins.typeUnwrap "array"
    
    [<Builtin("array?")>]
    static member array_check = Builtins.typeCheck "array"
    
    [<Builtin("print")>]
    static member print = Func(fun v ->
        printf "%s" (v.ToString())
        Unit
    )
    
    [<Builtin("printn")>]
    static member printn = Func(fun v ->
        printfn "%s" (v.ToString())
        Unit
    )

    [<Builtin("str")>]
    static member tostring = Func(fun v -> 
        String(v.ToString())
    )

    [<Builtin("int")>]
    static member toint = Func(fun v ->
        match v with
        | String(n) -> Int(int n)
        | _ -> typeFail "string" (v.Typename)
    )

    [<Builtin("readline")>]
    static member readline = Func(fun v ->
         ofType "unit" v |> ignore
         let line = Console.ReadLine()
         String(line)
    )

    [<Builtin("+")>]
    static member add = Func(fun a ->
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
    static member subtr = Func(fun a ->
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
    static member times = Func(fun a ->
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
    static member div = Func(fun a ->
        Func(fun b ->
            match a, b with
            | Int(x), Int(y) -> Int(x/y)
            | Float(x), Float(y) -> Float(x/y)
            | Int(x), _ -> typeFail "int" (b.Typename)
            | Float(x), _ -> typeFail "float" (b.Typename)
            | _ -> typeFail "int" (a.Typename)
        )
    )
    
    [<Builtin("^")>]
    static member exp = Func(fun a ->
        Func(fun b ->
            match a, b with
            | Int(x), Int(y) -> Int(pown x y)
            | Float(x), Float(y) -> Float(x**y)
            | Int(x), _ -> typeFail "int" (b.Typename)
            | Float(x), _ -> typeFail "float" (b.Typename)
            | _ -> typeFail "int" (a.Typename)
        )
    )

    [<Builtin("mod")>]
    static member modulo = Func(fun a ->
        Func(fun b ->
            match ofType "int" a, ofType "int" b with
            | Int(x), Int(y) -> Int(x % y)
        )
    )

    [<Builtin("<=")>]
    static member leq = Func(fun a ->
        Func(fun b ->
            match a, b with
            | Int(x), Int(y) -> Builtins.church (x <= y)
            | Float(x), Float(y) -> Builtins.church (x <= y)
            | Int(x), _ -> typeFail "int" (b.Typename)
            | Float(x), _ -> typeFail "float" (b.Typename)
            | _ -> typeFail "int" (a.Typename)
        )
    )
    
    [<Builtin("==")>]
    static member eq = Func(fun a ->
        Func(fun b ->
            match a, b with
            | Int(x), Int(y) -> Builtins.church ((x = y))
            | Float(x), Float(y) -> Builtins.church ((x = y))
            | _ -> equalityUndefined (a.Typename) (b.Typename)
        )
    )

    [<Builtin("error")>]
    static member error = Func(fun msg ->
        failwithf "Runtime error: `%A`" (msg.ToString())
    )