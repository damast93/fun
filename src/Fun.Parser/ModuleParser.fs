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

// Parse module, recognize illegal redefinitions

let nameOf = function Fun(n,_,_) | Data(n) -> n 

let findDuplicate keys = 
      keys 
   |> List.groupBy id
   |> List.filter (fun (k,ks) -> List.length ks > 1)
   |> List.map (fun (k,s) -> k)
   |> List.tryHead

let moduleParser = parsec {
    let! definitions = many definition
    
    match findDuplicate (List.map nameOf definitions) with
    | Some(multiple) -> return! failFatally (sprintf "Invalid redefinition of fun `%s`" multiple)
    | None -> return Module(definitions)
}
