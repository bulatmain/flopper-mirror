namespace Flopper.Tokens

open Flopper.Values

[<AbstractClass>]
type Token() =
    class
    end

[<AbstractClass>]
type Statement() =
    inherit Token()
    abstract member Eval: Environment -> Value * Environment

[<AbstractClass>]
type Expression() =
    inherit Statement()
