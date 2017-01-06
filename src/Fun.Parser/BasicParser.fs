module internal Fun.Parser.BasicParser

// The Parser for the Fun language

open FParsec

// Whitespace & comment management, thanks to
// http://stackoverflow.com/questions/8405032

let whitespaceChars = [' '; '\t'; '\n'; '\r']
let spaceStr = anyOf whitespaceChars |>> string 

// TODO don't show comments as alternative
let lineComment = pstring "//" >>. restOfLine true
let multiLineComment = 
    (between 
        (pstring "/*")
        (pstring "*/")
        (charsTillString "*/" false System.Int32.MaxValue))

let whitespace = lineComment <|> multiLineComment <|> spaceStr
let ws = skipMany whitespace
let strws st = pstring st .>> ws

let parsec = new ParserCombinator()

// Parsing terms 
// Every parser will consume tailing insignificant whitespace (.>> ws)

// Tokens 

let keywords = [ "let"; "letrec"; "in"; "fun"; "data"; "="; "$"; "." ] |> Set.ofList

let ident = 
    attempt (parsec {
        let fstDigitParser = isNoneOf "\(){}[];,.\"0123456789 \t\r\n"
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


// Fun grammar

let lambda = parsec {
    let! args = strws "\\" >>. arglist .>> strws "."
    let! body = term
    return Lambda(args, body)
}

let application = 
    many1 atom |>> function
        | [] -> failwith "invalid applications"
        | [x] -> x
        | ls -> Application(ls)

let letExpr = parsec {
    let! lhs = strws "let" >>. arglist .>> strws "="
    let! rhs = (lambda <|> application) .>> strws "in"
    let! body = term
    match lhs with
    | f::args -> return Let(f,args,rhs,body)
    | _       -> failwith "Invalid binding"
}

let letRecExpr = parsec {
    let! lhs = strws "letrec" >>. arglist .>> strws "="
    let! rhs = (lambda <|> application) .>> strws "in"
    let! body = term
    match lhs with
    | f::args -> return Let(f,args,rhs,body)
    | _       -> failwith "Invalid binding"
}

let sequence =
    sepBy1 (application <|> lambda <|> letExpr <|> letRecExpr) (strws ";") |>> function
        | [] -> failwith "invalid applications"
        | [x] -> x
        | ls -> Sequence(ls)

atomref := 
      literal
  <|> identifier
  <|> between (strws "(") (strws ")") term
  <|> between (strws "{") (strws "}") (term |>> Delay)

termref := sequence