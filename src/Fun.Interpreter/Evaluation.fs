module Fun.Interpreter.Evaluation

open Fun.Syntax
open Fun.Interpreter.Context

type internal V = Fun.Semantics.Value

let rec eval (context : Context) = function
    | Unit -> V.Unit
    | Int(n) -> V.Int(n)
    | Float(f) -> V.Float(f)
    | String(s) -> V.String(s)

    | Identifier(v) -> Context.lookup v context
    | Definition(v) -> Context.lookupDefinition v context

    | LetIn(v,lhs,body) ->
        let lhsvalue = eval context lhs
        let newcontext = Context.bind v lhsvalue context
        eval newcontext body

    | LetRecIn(v,lhs,body) -> failwith "Not implemented" // TODO

    | Sequence(exprs) ->
        exprs
        |> List.map (eval context)
        |> List.last

    | Application(fexpr, xexpr) ->
        let f = eval context fexpr
        let x = eval context xexpr
        match f with
        | V.Func(impl) -> impl x

    | Lambda(v, body) ->
        V.Func (fun x ->
            let innerContext = Context.bind v x context
            eval innerContext body
        )