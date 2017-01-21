module internal Fun.Parser.ModuleParser

open Fun.Parser.Config
open Fun.Parser.TermParser

open FParsec

// Parsing a whole module

// Typenames start with uppercase letters
let firstChar = isUpper
let consecutiveChar t = (isLetter t) || (isDigit t) || (isAnyOf "_" t)
let typename = many1Satisfy2 firstChar consecutiveChar .>> ws

let funDefinition = parsec {
    let! lhs = strws1 "fun" >>. arglist .>> strws "="
    let! rhs = lambda <|> application
    match lhs with
    | f::args -> return Fun(f,args,rhs)
}

let dataDefinition = parsec {
    let! name = strws1 "data" >>. typename
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

// TODO Spot redefinitions *while* parsing
let moduleParser = parsec {
    let! definitions = many definition
    
    match findDuplicate (List.map nameOf definitions) with
    | Some(multiple) -> return! failFatally (sprintf "Invalid redefinition of `%s`" multiple)
    | None -> return Module(definitions)
}
