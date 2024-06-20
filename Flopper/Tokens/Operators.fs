namespace Flopper.Tokens

open Flopper.Values

[<AbstractClass>]
type Operator(exprs: Expression list) =
    inherit Expression()
    member this.exprs = exprs

    override this.ToString() =
        let className = this.GetType().Name
        $"%s{className}({this.exprs})"

type Assign(id: Expression, expr: Expression) =
    inherit Operator([ id; expr ])
    override this.Eval(env) = (VoidValue(), env)

[<AbstractClass>]
type ArithmeticOperator(leftExpr, rightExpr: Expression) =
    inherit Operator([ leftExpr; rightExpr ])
    abstract member Operation: Value * Value -> Value

    override this.Eval(env) =
        let left, _ = leftExpr.Eval(env)
        let right, _ = rightExpr.Eval(env)
        (this.Operation(left, right), env)

type Plus(leftExpr, rightExpr: Expression) =
    inherit ArithmeticOperator(leftExpr, rightExpr)

    override this.Operation(left, right) =
        match (left, right) with
        | RealValue l, RealValue r -> RealValue(l + r)
        | _ -> raise (System.Exception("PLUS: Wrong operands"))

type Minus(leftExpr, rightExpr: Expression) =
    inherit ArithmeticOperator(leftExpr, rightExpr)

    override this.Operation(left, right) =
        match (left, right) with
        | RealValue l, RealValue r -> RealValue(l - r)
        | _ -> raise (System.Exception("MINUS: Wrong operands"))

type Multiply(leftExpr, rightExpr: Expression) =
    inherit ArithmeticOperator(leftExpr, rightExpr)

    override this.Operation(left, right) =
        match (left, right) with
        | RealValue l, RealValue r -> RealValue(l * r)
        | _ -> raise (System.Exception("MULTIPLY: Wrong operands"))

type Divide(leftExpr, rightExpr: Expression) =
    inherit ArithmeticOperator(leftExpr, rightExpr)

    override this.Operation(left, right) =
        match (left, right) with
        | RealValue l, RealValue r -> RealValue(l / r)
        | _ -> raise (System.Exception("DIVIDE: Wrong operands"))

type IntDivide(leftExpr, rightExpr: Expression) =
    inherit ArithmeticOperator(leftExpr, rightExpr)

    override this.Operation(left, right) =
        match (left, right) with
        | RealValue l, RealValue r -> RealValue(l / r - (l / r) % 1.)
        | _ -> raise (System.Exception("INT DIVIDE: Wrong operands"))

[<AbstractClass>]
type ComparationOperator(leftExpr, rightExpr: Expression) =
    inherit Operator([ leftExpr; rightExpr ])
    abstract member Operation: Value * Value -> Value

    override this.Eval(env) =
        let left, _ = leftExpr.Eval(env)
        let right, _ = rightExpr.Eval(env)
        (this.Operation(left, right), env)

type Equal(leftExpr, rightExpr: Expression) =
    inherit ComparationOperator(leftExpr, rightExpr)

    override this.Operation(left, right) =
        match (left, right) with
        | RealValue l, RealValue r -> BoolValue(l = r)
        | BoolValue l, BoolValue r -> BoolValue(l = r)
        | StringValue l, StringValue r -> BoolValue(l = r)
        | _ -> raise (System.Exception("EQ: Wrong operands"))

type NotEqual(leftExpr, rightExpr: Expression) =
    inherit ComparationOperator(leftExpr, rightExpr)

    override this.Operation(left, right) =
        match (left, right) with
        | RealValue l, RealValue r -> BoolValue(l <> r)
        | BoolValue l, BoolValue r -> BoolValue(l <> r)
        | StringValue l, StringValue r -> BoolValue(l <> r)
        | _ -> raise (System.Exception("NE: Wrong operands"))

type GreaterThan(leftExpr, rightExpr: Expression) =
    inherit ComparationOperator(leftExpr, rightExpr)

    override this.Operation(left, right) =
        match (left, right) with
        | RealValue l, RealValue r -> BoolValue(l > r)
        | BoolValue l, BoolValue r -> BoolValue(l > r)
        | StringValue l, StringValue r -> BoolValue(l > r)
        | _ -> raise (System.Exception("GT: Wrong operands"))

type GreaterOrEqual(leftExpr, rightExpr: Expression) =
    inherit ComparationOperator(leftExpr, rightExpr)

    override this.Operation(left, right) =
        match (left, right) with
        | RealValue l, RealValue r -> BoolValue(l >= r)
        | BoolValue l, BoolValue r -> BoolValue(l >= r)
        | StringValue l, StringValue r -> BoolValue(l >= r)
        | _ -> raise (System.Exception("GE: Wrong operands"))

type LessThan(leftExpr, rightExpr: Expression) =
    inherit ComparationOperator(leftExpr, rightExpr)

    override this.Operation(left, right) =
        match (left, right) with
        | RealValue l, RealValue r -> BoolValue(l < r)
        | BoolValue l, BoolValue r -> BoolValue(l < r)
        | StringValue l, StringValue r -> BoolValue(l < r)
        | _ -> raise (System.Exception("LT: Wrong operands"))

type LessOrEqual(leftExpr, rightExpr: Expression) =
    inherit ComparationOperator(leftExpr, rightExpr)

    override this.Operation(left, right) =
        match (left, right) with
        | RealValue l, RealValue r -> BoolValue(l <= r)
        | BoolValue l, BoolValue r -> BoolValue(l <= r)
        | StringValue l, StringValue r -> BoolValue(l <= r)
        | _ -> raise (System.Exception("LE: Wrong operands"))

[<AbstractClass>]
type LogicOperator(exprs: Expression list) =
    inherit Operator(exprs)

type And(leftExpr, rightExpr: Expression) =
    inherit LogicOperator([ leftExpr; rightExpr ])

    override this.Eval(env) =
        let left, _ = leftExpr.Eval(env)
        let right, _ = rightExpr.Eval(env)

        match (left, right) with
        | BoolValue l, BoolValue r -> (BoolValue(l && r), env)
        | _ -> raise (System.Exception("AND: Wrong operands"))

type Or(leftExpr, rightExpr: Expression) =
    inherit LogicOperator([ leftExpr; rightExpr ])

    override this.Eval(env) =
        let left, _ = leftExpr.Eval(env)
        let right, _ = rightExpr.Eval(env)

        match (left, right) with
        | BoolValue l, BoolValue r -> (BoolValue(l || r), env)
        | _ -> raise (System.Exception("OR: Wrong operands"))

type Not(expr: Expression) =
    inherit LogicOperator([ expr ])

    override this.Eval(env) =
        let res, _ = expr.Eval(env)

        match res with
        | BoolValue res -> (BoolValue(res = false), env)
        | _ -> raise (System.Exception("NOT: Wrong operand"))
