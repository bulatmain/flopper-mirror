namespace Flopper

module Values =
    type Environment = Map<string, Value>

    and Value =
        | RealValue of double
        | StringValue of string
        | BoolValue of bool
        | FunctionValue of (string * Value list -> Value)
        | VoidValue of unit
        | ErrorValue of string
