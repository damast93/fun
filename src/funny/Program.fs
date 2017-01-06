open System

open Fun
open Fun.Parser
open Fun.Interpreter

let runLine ln = 
    let parseResult = Parser.parse ln
    match parseResult with
    | Parser.Successful(term) ->
        printfn "%A\n" term
    | Parser.Error(err) ->
        printfn "%s\n" err

let rec repl() = 
    printf "> "
    let ln = Console.ReadLine()
    if (ln.Trim().ToLower() <> ":q") then
        runLine ln
        repl()

repl()