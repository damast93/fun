module Fun.Parser.Parser

open FParsec

open Fun.Parser.Config
open Fun.Parser.TermParser
open Fun.Parser.ModuleParser

type ParserResult<'a> = 
    | Successful of 'a
    | Error of string
    
let parse s = 
    match run (ws >>. moduleparser .>> eof) s with
    | Success(r,_,_) -> Successful(r)
    | Failure(s,_,_ ) -> Error(s)