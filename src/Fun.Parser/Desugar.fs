module Fun.Parser.Desugar

type internal E = Fun.Syntax.Expression

// Desugar terms

let internal curry args body = List.foldBack (fun x b -> E.Lambda(x,b)) args body

let internal delay e = 
    E.Lambda("$", E.Sequence
      [
        E.Application(E.Definition("unit!"), E.Identifier("$"));
        e
      ])

let internal consList exps =
    List.foldBack (fun x0 xs -> 
        E.Application(E.Application(E.Definition("cons"), x0), xs)
      ) exps (E.Definition("nil"))

let rec simplifyTerm = function
    | Unit -> E.Unit
    | Int(n) -> E.Int(n)
    | Float(x) -> E.Float(x)
    | String(s) -> E.String(s)

    | Identifier(v) -> E.Identifier(v)
    | Lambda(args, body) -> curry args (simplifyTerm body)
    | Sequence(ts) -> E.Sequence(List.map simplifyTerm ts)
    | Application(args) -> 
        args
        |> List.map simplifyTerm
        |> List.reduce (fun f x -> E.Application(f,x))

    | Let(f,args,rhs,body) ->
        E.LetIn(f, curry args (simplifyTerm rhs), simplifyTerm body)    
    | LetRec(f,args,rhs,body) ->
        E.LetRecIn(f, curry args (simplifyTerm rhs), simplifyTerm body)
    | Delay(t) -> delay (simplifyTerm t)
    | List(ts) -> consList (List.map simplifyTerm ts)

// Desugar modules

let internal isFun = function Fun(_) -> true | _ -> false
let internal isData = function Data(_) -> true | _ -> false
let internal nameOf = function Fun(n,_,_) | Data(n) -> n 

let simplifyModule (Module(definitions)) : Fun.Syntax.Module = 
    let funDefs = List.filter isFun definitions
    let dataDefs = List.filter isData definitions

    let usertypes = List.map nameOf dataDefs
    let definitions = 
           funDefs
        |> List.map (fun (Fun(f,args,body)) ->
            let lambda = curry args (simplifyTerm body)
            Fun.Syntax.Fun(f, lambda))

    { usertypes = usertypes; definitions = definitions }