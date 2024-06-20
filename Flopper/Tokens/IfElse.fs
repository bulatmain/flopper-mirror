namespace Flopper.Tokens

open Flopper.Values

type IfElse(condition: Expression, thenStatements: Statement list, elseStatements: Statement list) =
    inherit Statement()
    member this.condition = condition
    member this.thenStatements = thenStatements
    member this.elseStatements = elseStatements

    override this.ToString() =
        let className = this.GetType().Name
        $"%s{className} ({this.condition}, {this.thenStatements}, {this.elseStatements})"

    override this.Eval(env) =
        let cond, _ = condition.Eval env

        let statement =
            match cond with
            | BoolValue(true) -> thenStatements
            | BoolValue(false) -> elseStatements
            | _ -> raise (System.Exception("IF: Invalid condition type"))

        let init = (VoidValue(), env)

        let res, _ =
            (init, statement)
            ||> List.fold (fun acc x ->
                let _, newEnv = acc
                x.Eval(newEnv))

        (res, env)
