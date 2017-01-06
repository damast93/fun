module internal Fun.Parser.BasicParser

// The Parser for the Fun language

open FParsec

// Whitespace & comment management, thanks to
// http://stackoverflow.com/questions/8405032

let whitespaceChars = [' '; '\t'; '\n'; '\r']
let spaceStr = anyOf whitespaceChars |>> string 

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

let keywords = [ "let"; "letrec"; "in"; "fun"; "data"; "="; "$"; "." ] |> Set.ofList

let ident = attempt (parsec {
        let fstDigitParser = isNoneOf "\(){}[];,\"0123456789 \t\r\n"
        let! str = many1Satisfy2L fstDigitParser (isNoneOf "\()[],;{}\" \t\r\n") "identifier" .>> ws
        if keywords |> Set.contains str
          then return! fail("Reserved keyword")
          else return str
    })

let stringliteral = (pstring "\"") >>. (charsTillString "\"" true System.Int32.MaxValue) .>> ws |>> String
let identifier = ident |>> Identifier
let integer  = many1Satisfy isDigit .>> ws |>> (fun s -> Int(int s)) // TODO Include negative numbers 

// TODO Has to be there
let parse s =  match run ws s with _ -> ()