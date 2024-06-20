namespace Flopper

module Interpreter =
    let interpret (input: list<Tokens.Statement>) : Values.Value =
        let init = (Values.VoidValue(), Map.empty)

        try
            let res, _ =
                (init, input)
                ||> List.fold (fun acc x ->
                    let _, newEnv = acc
                    x.Eval(newEnv))

            res
        with ex ->
            Values.ErrorValue(ex.Message)
