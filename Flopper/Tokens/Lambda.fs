namespace Flopper.Tokens

open Flopper.Values

type Lambda(args: Arguments list, statements: Statement list) =
    inherit Expression()
    member this.args = args
    member this.statements = statements

    override this.ToString() =
        let className = this.GetType().Name
        $"%s{className} ({this.args}, {this.statements})"

    override this.Eval(env) =
        let rec lambda (this': string, argsValues: Value list) =
            let argsNames =
                args
                |> List.map (fun x ->
                    let res, _ = x.Eval(env)

                    match res with
                    | StringValue s -> s
                    | _ -> raise (System.Exception($"LAMBDA: {this'} has invalid argument {string res}")))

            let newEnv =
                (env, List.zip argsNames argsValues |> Map.ofList)
                ||> Map.fold (fun acc key value -> acc.Add(key, value))

            let init = (VoidValue(), newEnv.Add(this', FunctionValue(lambda)))

            let res, _ =
                (init, statements)
                ||> List.fold (fun acc x ->
                    let _, newEnv = acc
                    x.Eval(newEnv))

            res

        (FunctionValue(lambda), env)
