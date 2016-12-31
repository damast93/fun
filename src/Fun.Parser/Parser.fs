module Fun.Parser.Parser

open FParsec
open BasicParser

let parse s = 
    match run ws s with
    | _ -> ()
