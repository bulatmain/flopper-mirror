namespace Flopper.Tokens

[<AbstractClass>]
type Literals(record: string) =
    inherit Expression()
    member this.record = record

    override this.ToString() =
        let className = this.GetType().Name
        $"%s{className}(%s{this.record})"

type LiteralReal(record: string) =
    inherit Literals(record)

    override this.Eval(env) =
        (Flopper.Values.RealValue(double record), env)

type LiteralString(record: string) =
    inherit Literals(record)

    override this.Eval(env) =
        (Flopper.Values.StringValue(record), env)

type LiteralBool(record: string) =
    inherit Literals(record)

    override this.Eval(env) =
        match record with
        | "true" -> (Flopper.Values.BoolValue(true), env)
        | _ -> (Flopper.Values.BoolValue(false), env)
