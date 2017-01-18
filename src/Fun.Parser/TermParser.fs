module internal Fun.Parser.TermParser

open FParsec
open Fun.Parser.Config

// Parsing terms 
// Every parser will consume tailing insignificant whitespace (.>> ws)

// Tokens 

let keywords = [ "let"; "letrec"; "in"; "fun"; "data"; "="; "." ] |> Set.ofList

let ident = 
    attempt (parsec {
        let fstDigitParser = isNoneOf "\(){}[];,.$\"0123456789 \t\r\n"
        let! str = many1Satisfy2 fstDigitParser (isNoneOf "\()[].,;{}\" \t\r\n") .>> ws
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

let application = 
    many1 atom |>> function
        | [x] -> x
        | ls -> Application(ls)

let letExpr = parsec {
    let! lhs = strws "let" >>. arglist .>> strws "="
    let! rhs = (lambda <|> application) .>> strws "in"
    let! body = term
    match lhs with
    | f::args -> return Let(f,args,rhs,body)
}

let letRecExpr = parsec {
    let! lhs = strws "letrec" >>. arglist .>> strws "="
    let! rhs = (lambda <|> application) .>> strws "in"
    let! body = term
    match lhs with
    | f::args -> return Let(f,args,rhs,body)
}

let sequence =
    sepBy1 (application <|> lambda <|> letExpr <|> letRecExpr) (strws ";") |>> function
        | [x] -> x
        | ls -> Sequence(ls)

atomref := 
      literal
  <|> identifier
  <|> between (strws "(") (strws ")") term
  <|> between (strws "{") (strws "}") (term |>> Delay)

termref := sequence