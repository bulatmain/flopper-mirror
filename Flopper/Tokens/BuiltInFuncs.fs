namespace Flopper.Tokens

open Flopper.Values

[<AbstractClass>]
type BuiltInFuncs() =
    inherit Expression()

type ReadReal() =
    inherit BuiltInFuncs()

    override this.Eval(env) =
        try
            let input = System.Console.ReadLine()
            (RealValue(double input), env)
        with _ ->
            raise (System.Exception("READ_REAL: Invalid input value"))

type ReadString() =
    inherit BuiltInFuncs()

    override this.Eval(env) =
        let input = System.Console.ReadLine()
        (StringValue(input), env)

type Print(exprs: Expression list) =
    inherit BuiltInFuncs()
    member this.exprs = exprs

    override this.Eval(env) =
        let rec print (exprs: Expression list) =
            match exprs with
            | [] -> ()
            | expr :: tail ->
                let res =
                    match expr.Eval(env) with
                    | RealValue n, _ -> string n
                    | StringValue s, _ -> s
                    | BoolValue b, _ -> string b
                    | res, _ -> string res

                printf $"{res} "
                print tail


        print exprs
        printfn ""
        (VoidValue(), env)
