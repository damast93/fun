module internal Fun.Parser.Config

open FParsec

// Whitespace & comment management, thanks to
// http://stackoverflow.com/questions/8405032

let whitespaceChars = [' '; '\t'; '\n'; '\r']
let spaceStr : Parser<string,unit> = anyOf whitespaceChars |>> string 

// TODO don't show comments as alternative in error messages
let lineComment = pstring "//" >>. restOfLine true
let multiLineComment = 
    (between 
        (pstring "/*")
        (pstring "*/")
        (charsTillString "*/" false System.Int32.MaxValue))

let whitespace = lineComment <|> multiLineComment <|> spaceStr
let ws = skipMany whitespace

let strws st = pstring st .>> ws
let strws1 st = pstring st >>. whitespace >>. ws

// Expression builder
let parsec = new ParserCombinator()

// Helpers
let skip p = p >>. preturn ()