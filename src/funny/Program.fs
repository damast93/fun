open System

open Fun
open Fun.Parser
open Fun.Interpreter 

let runLine ln = 
    let parseResult = Parser.parseExpression ln
    match parseResult with
    | Parser.Successful(expr) ->
        printfn "%A\n" expr
    | Parser.Error(err) ->
        printfn "%s\n" err

let rec repl() = 
    printf "> "
    let ln = Console.ReadLine()
    if (ln.Trim().ToLower() <> ":q") then
        runLine ln
        repl()

let loadFile(fn) = 
    printfn "[Loading %s]" fn
    let contents = System.IO.File.ReadAllText(fn)
    match Parser.parseModuleSimplified contents with
    | Parser.Successful(term) -> 
        printfn "%A\n" term
    | Parser.Error(err) ->
        printfn "%s\n" err

    printfn "\n"
    repl()

let main() =
    let args = Environment.GetCommandLineArgs() 
    match args with
    | [|_|] -> repl()
    | [|_; fn|] -> loadFile(fn)
    | _ -> 
        printfn "Unrecognized options ..."
        repl()

main()