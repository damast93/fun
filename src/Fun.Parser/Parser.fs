module Fun.Parser.Parser

open FParsec

open Fun.Parser.Config
open Fun.Parser.TermParser
open Fun.Parser.ModuleParser

type ParserResult<'a> = 
    | Successful of 'a
    | Error of string

let internal wrap parser s = 
    match run parser s with
    | Success(r,_,_) -> Successful(r)
    | Failure(s,_,_ ) -> Error(s)

let internal wrapTight parser = wrap (ws >>. parser .>> eof)

let parseTerm s = wrapTight term s
let parseModule s = wrapTight moduleParser s