namespace Flopper.Tokens

open Flopper.Values

type Call(id: ID, exprs: Expression list) =
    inherit Expression()
    member this.id = id
    member this.exprs = exprs

    override this.ToString() =
        let className = this.GetType().Name
        $"%s{className}({this.id}, {this.exprs})"

    override this.Eval(env) =
        match id.name with
        | "print" -> Print(exprs).Eval(env)
        | "read_real" -> ReadReal().Eval(env)
        | "read_string" -> ReadString().Eval(env)
        | _ ->
            let vals =
                exprs
                |> List.map (fun x ->
                    let res, _ = x.Eval(env)
                    res)

            let f, _ = id.Eval(env)

            match f with
            | FunctionValue f -> (f (id.name, vals), env)
            | _ -> raise (System.Exception($"CALL: {id.name} is not a function"))
