module Fun.Parser.Parser

open FParsec
open BasicParser

type ParserResult<'a> = 
    | Successful of 'a
    | Error of string
    
let parse s = 
    match run (ws >>. (identifier <|> integer)) s with
    | Success(r,_,_) -> Successful(r)
    | Failure(s,_,_ ) -> Error(s)