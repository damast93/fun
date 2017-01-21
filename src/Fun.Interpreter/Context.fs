module internal Fun.Interpreter.Context

open Fun.Semantics
open Fun.Interpreter.Exceptions

type Context = {
    bindings : Map<string,Value ref>
    definitions : Map<string,Value ref>
}

let empty : Context = { bindings = Map.empty; definitions = Map.empty }

let private lookupRef ident (context : Context) = 
    match Map.tryFind ident context.bindings with
    | Some(value) -> value
    | None ->
        match Map.tryFind ident context.definitions with
        | Some(value) -> value
        | None -> notInScopeFail ident
        
let lookup ident context = !(lookupRef ident context) 

let lookupDefinition ident (context : Context) = 
    match Map.tryFind ident context.definitions with
    | Some(value) -> !value
    | None -> notInScopeFail ident

let bind name value context = 
    { context with bindings = Map.add name (ref value) context.bindings }

let define name value context = 
    { context with definitions = Map.add name (ref value) context.definitions }

let updateRef ident value context = 
   let reference = lookupRef ident context
   reference := value