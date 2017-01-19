open System

open Fun
open Fun.Parser
open Fun.Interpreter 

let runLine ln = 
    let parseResult = Parser.parseExpression ln
    match parseResult with
    | Parser.Error(err) ->
        printfn "%s\n" err
    | Parser.Successful(expr) ->
        let context = Context.empty
        let result = Evaluation.eval context expr
        printfn "%A" expr
        printfn "%A\n" result

let rec repl() = 
    printf "> "
    let ln = Console.ReadLine()
    if (ln.Trim().ToLower() <> ":q") then
        runLine ln
        repl()

let loadFile(fn) = 
    printfn "[Loading %s]" fn
    let contents = System.IO.File.ReadAllText(fn)
    match Parser.parseModule contents with
    | Parser.Error(err) ->
        printfn "%s\n" err
    | Parser.Successful(mdl) -> 
        printfn "[Loaded %s, %i definitions, %i user types]" fn (List.length mdl.definitions) (List.length mdl.usertypes)
    printfn ""
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