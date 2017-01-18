module internal Fun.Parser.ModuleParser

open Fun.Parser.Config
open Fun.Parser.TermParser

open FParsec

// Parsing a whole module

// TODO Work out a good definition of typenames
let typename = many1Chars letter .>> ws

let funDefinition = parsec {
    let! lhs = strws "fun" >>. arglist .>> strws "="
    let! rhs = lambda <|> application
    match lhs with
    | f::args -> return Fun(f,args,rhs)
}

let dataDefinition = parsec {
    let! name = strws "data" >>. typename
    return Data(name)
}

let definition = funDefinition <|> dataDefinition

let moduleparser = many definition
//
//let isFun = function Fun(_) -> true | _ -> false
//let isData = function Data(_) -> true | _ -> false
//
//let findDuplicates keys = 
//      keys 
//   |> List.groupBy id
//   |> List.filter (fun (k,ks) -> List.length ks > 1)
//   |> List.map (fun (k,s) -> k)
//   |> List.tryHead
//
//
//let validateModule definitions = 
//    let funDecls = List.filter isFun definitions
//    let dataDecls = List.filter isData definitions
//
//    match findDuplicates (List.map (fun (Fun(fn,_,_)) -> fn) funDecls) with
//    | Some(multiple) -> failwithf "Invalid redefinition of fun `%s`" multiple
//    | None -> ()
//
//    match findDuplicates (List.map (fun (Data(dt)) -> dt) dataDecls) with
//    | Some(multiple) -> failwithf "Invalid redefinition of datatype `%s`" multiple
//    | None -> ()

