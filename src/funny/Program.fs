open System

open Fun
open Fun.Parser
open Fun.Interpreter 

type Interactive() = 
    let interpreter = new Interpreter()

    member this.LoadFile(fn) = 
        printfn "[Loading %s ...]" fn
        let contents = System.IO.File.ReadAllText(fn)

        match Parser.parseModule contents with
        | Parser.Error(err) ->
            printfn "%s\n" err
        | Parser.Successful(mdl) -> 
            try
                interpreter.LoadModule(mdl)
                printfn "[Loaded %s, %i definitions, %i user types]" fn (List.length mdl.definitions) (List.length mdl.usertypes)
            with ex ->
                printfn "!> %s" (ex.Message)

    member this.RunLine(line) =
        match Parser.parseExpression line with
        | Parser.Error(err) ->
            printfn "%s\n" err
        | Parser.Successful(expr) ->
            let result = interpreter.Evaluate(expr)
            printfn "%s" (result.ToString())

    member this.Repl() = 
        printf "\n> "
        let line = Console.ReadLine()
        if line.Trim().StartsWith(":") then
            let option = line.Trim().Substring(1)
            this.PerformOption(option)
        else
            try
                this.RunLine(line)
            with ex ->
                printfn "!> %s" (ex.Message)
            this.Repl()

    member this.PerformOption(option) = 
        match option with
        | option when "quit".StartsWith(option) -> () 
        | option when "browse".StartsWith(option) || "info".StartsWith(option) -> 
            printfn "Types:\n%s\n" (String.concat ", " interpreter.UserTypes)
            printfn "Globals:\n%s" (String.concat ", " interpreter.Definitions)
            this.Repl()
        | option when List.exists (option.StartsWith) ["l ";"load "; "m "; "module "] ->
            let index = option.IndexOf(" ")
            let fn = option.Substring(index).Trim()
            this.LoadFile(fn)
            this.Repl()
        | _ ->
            printfn "Unrecognized option"
            this.Repl()


let main() =
    let args = Environment.GetCommandLineArgs() 
    let interactive = new Interactive()
    printf "Funny - Fun interactive interpreter\n"

    match args with
    | [|_|] ->
        interactive.LoadFile("core.fun")
        interactive.Repl()
    | [|_; fn|] -> 
        interactive.LoadFile("core.fun")
        interactive.LoadFile(fn)
        interactive.Repl()
    | _ -> 
        printfn "Unrecognized options ..."
        interactive.Repl()

main()