module internal Fun.Parser.TermParser

open FParsec
open Fun.Parser.Config

// Parsing terms 
// Every parser will consume tailing insignificant whitespace (.>> ws)

// Tokens 
// TODO Allow definition of infix operators

let keywords = [ "let"; "letrec"; "in"; "fun"; "data"; "="; "." ] |> Set.ofList

let firstChar = isLetter
let consecutiveChar = isNoneOf "\()[].,;{}\" \t\r\n"
let operatorChar = isAnyOf "+-*/.<>=%$§°!?~`^#'_|&"

let operator = attempt ((many1Satisfy operatorChar) .>> ws)

let normalIdent = many1Satisfy2 firstChar consecutiveChar .>> ws
let parentizedOperator =  strws "(" >>. operator .>> strws ")"

let ident = 
    attempt (parsec {
        let! str = normalIdent <|> parentizedOperator
        if keywords |> Set.contains str
            then return! fail("Reserved keyword")
            else return str
    }) <?> "identifier"

// Literals

let atom, atomref = createParserForwardedToRef<Term, unit>()
let term, termref = createParserForwardedToRef<Term, unit>()

let identifier    = ident |>> Identifier
let unitLiteral   = attempt (strws "(" >>. strws ")") |>> fun _ -> Unit
let stringLiteral = (pstring "\"") >>. (charsTillString "\"" true System.Int32.MaxValue) .>> ws |>> String // TODO consume string greedily
let integer       = many1Satisfy isDigit .>> ws |>> (fun s -> Int(int s)) <?> "integer" // TODO Include negative numbers 

let list          = between (strws "[") (strws "]") (sepBy1 atom (strws ",")) |>> List

let literal       = unitLiteral <|> stringLiteral <|> list <|> integer // TODO include floats
let arglist       = many1 ident


// Term grammar

let lambda = parsec {
    let! args = strws "\\" >>. arglist .>> strws "."
    let! body = term
    return Lambda(args, body)
}

let applications = 
    many1 atom |>> function
        | [x] -> x
        | ls -> Application(ls)

let arithmetic = parsec {
    let! op1 = applications
    let! summands = many (operator .>>. applications)
    let expr = List.fold (fun lhs (op, rhs) -> Application([Identifier(op); lhs; rhs])) op1 summands
    return expr
}

let letExpr = parsec {
    let! lhs = strws1 "let" >>. arglist .>> strws "="
    let! rhs = (lambda <|> arithmetic) .>> strws1 "in"
    let! body = term
    match lhs with
    | f::args -> return Let(f,args,rhs,body)
}

let letRecExpr = parsec {
    let! lhs = strws1 "letrec" >>. arglist .>> strws "="
    let! rhs = (lambda <|> arithmetic) .>> strws1 "in"
    let! body = term
    match lhs with
    | f::args -> return LetRec(f,args,rhs,body)
}

let sequence =
    sepBy1 (arithmetic <|> lambda <|> letRecExpr <|> letExpr) (strws ";") |>> function
        | [x] -> x
        | ls -> Sequence(ls)

atomref := 
      literal
  <|> identifier
  <|> between (strws "(") (strws ")") term
  <|> between (strws "{") (strws "}") (term |>> Delay)

termref := sequence