namespace Fun.Interpreter

open Fun.Syntax
open Fun.Interpreter.Context
open Fun.Interpreter.Runtime

type internal V = Fun.Semantics.Value

type Interpreter() as this = 

    let mutable userTypes = Set.empty
    let mutable definitions = Set.empty

    let mutable globals = Context.empty

    do
        userTypes <- Set.ofSeq Builtins.builtin_types
        for typename in Builtins.builtin_types do
            this.AddType(typename)

            globals <- Context.define "true" Builtins.bool_true globals
            globals <- Context.define "false" Builtins.bool_false globals
            globals <- Context.define "unit!" Builtins.unit_unwrap globals
            globals <- Context.define "unit?" Builtins.unit_check globals
            globals <- Context.define "print" Builtins.print globals

    member this.UserTypes = userTypes |> Set.toSeq
    member this.Definitions = definitions |> Set.toSeq

    member this.Item
        with get(vname) = Context.lookupDefinition vname globals

    member this.LoadModule(mdl : Module) = 
        match mdl.definitions |> List.tryFind (fun (Fun(f,_)) -> Set.contains f definitions) with
        | Some(Fun(f,_)) -> failwithf "Invalid redefinition of fun `%s`" f
        | None -> () // continue happily
        
        match mdl.usertypes |> List.tryFind (fun t-> Set.contains t userTypes) with
        | Some(t) -> failwithf "Invalid redefinition of data `%s`" t
        | None -> () // continue happily

        mdl.usertypes |> List.iter (this.AddType)
        this.LoadDefinitions(mdl.definitions)


    member private this.AddType(typename) = 
        userTypes <- Set.add typename userTypes

        let ctor = Runtime.Builtins.typeConstructor typename
        globals <- Context.define typename ctor globals

        let unwrap = Runtime.Builtins.typeUnwrap typename
        globals <- Context.define (typename + "!") unwrap globals

        let check = Runtime.Builtins.typeCheck typename
        globals <- Context.define (typename + "?") check globals

    member private this.LoadDefinitions(defs) =        
        let funcs = defs |> List.map (fun (Fun(fn,_)) -> fn)

        // Add dummy value for mutual recursion
        for fn in funcs do
            globals <- Context.define fn V.Unit globals

        // Evaluate and assign functions
        for (Fun(fn,body)) in defs do
            let f = Evaluation.eval globals body
            Context.updateRef fn f globals

        // Remember the definitions
        for fn in funcs do
            definitions <- Set.add fn definitions

    member this.Evaluate(expr : Expression) = 
        Evaluation.eval globals expr