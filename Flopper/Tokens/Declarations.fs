namespace Flopper.Tokens

[<AbstractClass>]
type Declarations(id: ID, expr: Expression) =
    inherit Statement()
    member this.id = id
    member this.expr = expr

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
        | true -> raise (System.Exception($"DECLARATION: Invalid variable name {id.name}"))
        | _ ->
            let res, _ = expr.Eval(env)
            (res, env.Add(id.name, res))

type DeclarationReal(id: ID, expr: Expression) =
    inherit Declarations(id, expr)

    override this.ToString() =
        let className = this.GetType().Name
        $"%s{className}({this.id}, {this.expr})"

type DeclarationString(id: ID, expr: Expression) =
    inherit Declarations(id, expr)

    override this.ToString() =
        let className = this.GetType().Name
        $"%s{className}({this.id}, {this.expr})"

type DeclarationBool(id: ID, expr: Expression) =
    inherit Declarations(id, expr)

    override this.ToString() =
        let className = this.GetType().Name
        $"%s{className}({this.id}, {this.expr})"

type DeclarationFunc(id: ID, expr: Expression) =
    inherit Declarations(id, expr)

    override this.ToString() =
        let className = this.GetType().Name
        $"%s{className}({this.id}, {this.expr})"
