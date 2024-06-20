namespace Flopper.Tokens

type ID(name: string) =
    inherit Expression()
    member this.name = name

    override this.ToString() =
        let className = this.GetType().Name
        $"%s{className}(%s{this.name})"

    override this.Eval(env) =
        let keyWords =
            [ "print"
              "read_real"
              "read_string"
              "real"
              "string"
              "bool"
              "func"
              "lambda"
              "if"
              "else" ]

        match keyWords |> List.exists (fun x -> x = name) with
        | true -> raise (System.Exception($"ID: Invalid variable or function name {name}"))
        | _ ->
            try
                (env.Item(name), env)
            with _ ->
                raise (System.Exception($"ID: No such variable or function: {name}"))
