module internal Fun.Interpreter.DependencyChecker

open Fun.Syntax
open Fun.Interpreter.Context

let rec unresolved globals bindings = function
    | Unit -> []
    | Int(_) -> []
    | Float(_) -> []
    | String(_) -> []
    | Identifier(v) -> 
        if Set.contains v globals || Set.contains v bindings
            then []
            else [v]
    | Definition(v) ->
        if Set.contains v globals
            then []
            else [v]
    | LetIn(v,lhs,body) ->
        let lhsUnresolved  = unresolved globals bindings lhs
        let bodyUnresolved = unresolved globals (Set.add v bindings) body
        lhsUnresolved @ bodyUnresolved
    | LetRecIn(v,lhs,body) ->
        let bindings2 = Set.add v bindings
        let lhsUnresolved  = unresolved globals bindings2 lhs
        let bodyUnresolved = unresolved globals bindings2 body
        lhsUnresolved @ bodyUnresolved
    | Sequence(exps) -> 
          List.collect (unresolved globals bindings) exps
    | Application(f,x) ->
        (unresolved globals bindings f) @ (unresolved globals bindings x)
    | Lambda(v, body) ->
        unresolved globals (Set.add v bindings) body


