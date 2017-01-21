module internal Fun.Interpreter.Evaluation

open Fun.Syntax
open Fun.Interpreter.Context
open Fun.Interpreter.Exceptions

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

    | LetRecIn(v,lhs,body) -> 
        let dummyContext = Context.bind v (V.Unit) context
        let lhsvalue = eval dummyContext lhs
        Context.updateRef v lhsvalue dummyContext
        eval dummyContext body

    | Sequence(exprs) ->
        exprs
        |> List.map (eval context)
        |> List.last

    | Application(fexpr, xexpr) ->
        let f = eval context fexpr
        let x = eval context xexpr
        match ofType "function" f with
        | V.Func(impl) -> impl x

    | Lambda(v, body) ->
        V.Func (fun x ->
            let innerContext = Context.bind v x context
            eval innerContext body
        )