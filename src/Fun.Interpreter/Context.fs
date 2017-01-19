module Fun.Interpreter.Context

open Fun.Semantics

type Context = {
    bindings : Map<string,Value>
    definitions : Map<string,Value>
}

let empty : Context = { bindings = Map.empty; definitions = Map.empty }

let lookup ident (context : Context) = 
    match Map.tryFind ident context.bindings with
    | Some(value) -> value
    | None ->
        match Map.tryFind ident context.definitions with
        | Some(value) -> value
        | None -> failwithf "Binding `%s` is not defined in the current scope" ident

let lookupDefinition ident (context : Context) = 
    match Map.tryFind ident context.definitions with
    | Some(value) -> value
    | None -> failwithf "Binding `%s` is not defined in the current scope" ident

let bind name value context = 
    { context with bindings = Map.add name value context.bindings }