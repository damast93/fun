module Fun.Interpreter.Context

open Fun.Semantics

type Context = internal {
    bindings : Map<string,Value ref>
    definitions : Map<string,Value ref>
}

let empty : Context = { bindings = Map.empty; definitions = Map.empty }

let internal lookupRef ident (context : Context) = 
    match Map.tryFind ident context.bindings with
    | Some(value) -> value
    | None ->
        match Map.tryFind ident context.definitions with
        | Some(value) -> value
        | None -> failwithf "Binding `%s` is not defined in the current scope" ident
        
let lookup ident context = !(lookupRef ident context) 

let lookupDefinition ident (context : Context) = 
    match Map.tryFind ident context.definitions with
    | Some(value) -> !value
    | None -> failwithf "Binding `%s` is not defined in the current scope" ident

let bind name value context = 
    { context with bindings = Map.add name (ref value) context.bindings }

let internal updateRef ident value context = 
   let reference = lookupRef ident context
   reference := value