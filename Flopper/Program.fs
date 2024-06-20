open Flopper

[<EntryPoint>]
let main(args) =
    if args.Length <> 1 then
        printf "Error: there should be one code file path argument\n"

    // let input_file = System.Console.ReadLine()
    let input_file = args[0]
    let input = System.IO.File.ReadAllText input_file

    let result = Parser.parse input

    match result with
    | Result.Ok res ->
        // AST output:
        // printfn $"{res}"
        let exitCode = Interpreter.interpret res
        printfn $"Program finished with {exitCode}"
        0
    | Result.Error err -> 
        printf $"{err}"
        -1
