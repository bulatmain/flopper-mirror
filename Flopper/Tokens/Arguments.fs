namespace Flopper.Tokens

[<AbstractClass>]
type Arguments(id: ID) =
    inherit Statement()
    member this.id = id

    override this.ToString() =
        let className = this.GetType().Name
        $"%s{className}({this.id})"

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

        match keyWords |> List.exists (fun x -> x = id.name) with
        | true -> raise (System.Exception($"ARGUMENTS: Invalid argument name {id.name}"))
        | _ -> (Flopper.Values.StringValue(id.name), env)

type ArgumentReal(id: ID) =
    inherit Arguments(id)

type ArgumentString(id: ID) =
    inherit Arguments(id)

type ArgumentFunc(id: ID) =
    inherit Arguments(id)

type ArgumentBool(id: ID) =
    inherit Arguments(id)
