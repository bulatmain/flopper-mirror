namespace Flopper

open FParsec

module Parser =
    let ws: Parser<_, unit> = skipMany ((skipChar ' ') <|> (skipChar '\t'))
    let wsn = skipMany ((skipChar ' ') <|> (skipChar '\t') <|> (skipChar '\n'))

    let quote: Parser<_, unit> = skipChar '\"'

    let literalString: Parser<Tokens.LiteralString, unit> =
        quote >>. manyCharsTill anyChar quote |>> Tokens.LiteralString .>> ws

    let literalReal: Parser<Tokens.LiteralReal, unit> =
        pfloat |>> string |>> Tokens.LiteralReal .>> ws

    let literalBool: Parser<Tokens.LiteralBool, unit> =
        pstring "true" <|> pstring "false" |>> Tokens.LiteralBool .>> ws

    let castToLiteral l = l :> Tokens.Literals

    let literal =
        choice
            [ literalString |>> castToLiteral
              literalReal |>> castToLiteral
              literalBool |>> castToLiteral ]

    let identifierFirstChar: Parser<char, unit> = letter <|> pchar '_'
    let identifierRestChar: Parser<char, unit> = letter <|> digit <|> pchar '_'

    let identifier: Parser<Tokens.ID, unit> =
        pipe2 (identifierFirstChar |>> string) (manyChars identifierRestChar) (fun first rest -> first + rest)
        |>> Tokens.ID
        .>> ws

    // A generic parser function for parsing elements of a tuple
    let parseElement<'T> (elementParser: Parser<'T, unit>) : Parser<'T, unit> = ws >>. elementParser .>> ws

    // A generic parser for a list of elements separated by a separator
    let parseSeparated<'T> (elementParser: Parser<'T, unit>) (separator: Parser<_, unit>) : Parser<'T list, unit> =
        sepBy (parseElement elementParser) separator

    // A generic parser for parsing tuples contained within parentheses using `between`
    let parseList<'T> (elementParser: Parser<'T, unit>) : Parser<'T list, unit> =
        between (pchar '(' .>> ws) (ws .>> pchar ')') (parseSeparated elementParser (pchar ',' .>> ws))

    let opParser = OperatorPrecedenceParser<Tokens.Expression, unit, unit>()

    let expression, expressionRef =
        createParserForwardedToRef<Tokens.Expression, unit> ()

    let expressionsSeparated: Parser<Tokens.Expression list, unit> =
        parseSeparated expression (pchar ',')

    let expressionsInBrackets: Parser<Tokens.Expression list, unit> =
        parseList expression

    let argumentToString _type = pstring _type .>> ws >>. identifier
    let argumentReal = argumentToString "real" |>> Tokens.ArgumentReal
    let argumentString = argumentToString "string" |>> Tokens.ArgumentString
    let argumentBool = argumentToString "bool" |>> Tokens.ArgumentBool
    let argumentFunc = argumentToString "func" |>> Tokens.ArgumentFunc

    let castToArgument a = a :> Tokens.Arguments

    let argument =
        choice
            [ argumentReal |>> castToArgument
              argumentString |>> castToArgument
              argumentBool |>> castToArgument
              argumentFunc |>> castToArgument ]

    let declToString _type =
        pipe2 ((argumentToString _type) .>> ws .>> pchar '=' .>> ws) expression

    let declReal = declToString "real" (fun id expr -> Tokens.DeclarationReal(id, expr))

    let declString =
        declToString "string" (fun id expr -> Tokens.DeclarationString(id, expr))

    let declBool = declToString "bool" (fun id expr -> Tokens.DeclarationBool(id, expr))
    let declFunc = declToString "func" (fun id expr -> Tokens.DeclarationFunc(id, expr))

    let castToDecl d = d :> Tokens.Declarations

    let decl =
        choice
            [ declReal |>> castToDecl
              declString |>> castToDecl
              declBool |>> castToDecl
              declFunc |>> castToDecl ]

    let castToStatement s = s :> Tokens.Statement

    let rec parseStatement () =
        parse {
            let! res =
                choice
                    [ attempt ifelse |>> castToStatement
                      attempt decl |>> castToStatement
                      expression |>> castToStatement ]

            return res
        }

    and statement = parseStatement ()

    and statementsList = (parseSeparated statement (pchar ';' .>> wsn))

    and statementsInBrackets =
        between (pchar '{' .>> wsn) (wsn .>> pchar '}') statementsList

    and ifelse =
        pipe3
            (pstring "if" .>> wsn .>> pchar '(' .>> wsn >>. expression
             .>> wsn
             .>> pchar ')'
             .>> wsn)
            (wsn >>. statementsInBrackets .>> wsn)
            (pstring "else" .>> wsn >>. statementsInBrackets .>> wsn)
            (fun expr ifStmts elseStmts -> Tokens.IfElse(expr, ifStmts, elseStmts))


    let lambda: Parser<Tokens.Lambda, unit> =
        (pstring "lambda" .>> wsn >>. (parseList argument)) .>> wsn
        .>>. statementsInBrackets
        .>> wsn
        |>> Tokens.Lambda


    let call: Parser<Tokens.Call, unit> =
        pipe2 identifier expressionsInBrackets (fun id exprs -> Tokens.Call(id, exprs))

    let bracketedExpression: Parser<Tokens.Expression, unit> =
        between (pchar '(' .>> ws) (ws .>> pchar ')') expression

    let castToExpression e = e :> Tokens.Expression

    let term: Parser<Tokens.Expression, unit> =
        parse {
            let _res =
                choice
                    [ attempt bracketedExpression
                      attempt lambda |>> castToExpression
                      attempt call |>> castToExpression
                      identifier |>> castToExpression
                      literal |>> castToExpression
                      ]

            let! res = _res .>> ws
            return res
        }

    // Set the term parser for the operator precedence parser
    opParser.TermParser <- term

    // Set using operators
    opParser.AddOperator(
        InfixOperator("*", ws, 1, Associativity.Left, (fun left right -> Tokens.Multiply(left, right)))
    )

    opParser.AddOperator(InfixOperator("/", ws, 2, Associativity.Right, (fun left right -> Tokens.Divide(left, right))))

    opParser.AddOperator(
        InfixOperator("//", ws, 3, Associativity.Right, (fun left right -> Tokens.IntDivide(left, right)))
    )

    opParser.AddOperator(InfixOperator("+", ws, 4, Associativity.Left, (fun left right -> Tokens.Plus(left, right))))
    opParser.AddOperator(InfixOperator("-", ws, 5, Associativity.Left, (fun left right -> Tokens.Minus(left, right))))

    opParser.AddOperator(InfixOperator("&", ws, 6, Associativity.Left, (fun left right -> Tokens.And(left, right))))
    opParser.AddOperator(InfixOperator("|", ws, 7, Associativity.Left, (fun left right -> Tokens.Or(left, right))))
    opParser.AddOperator(PrefixOperator("!", ws, 8, true, (fun expr -> Tokens.Not(expr))))

    opParser.AddOperator(InfixOperator("==", ws, 9, Associativity.Left, (fun left right -> Tokens.Equal(left, right))))

    opParser.AddOperator(
        InfixOperator("!=", ws, 10, Associativity.Left, (fun left right -> Tokens.NotEqual(left, right)))
    )

    opParser.AddOperator(
        InfixOperator("<", ws, 11, Associativity.None, (fun left right -> Tokens.LessThan(left, right)))
    )

    opParser.AddOperator(
        InfixOperator("<=", ws, 12, Associativity.None, (fun left right -> Tokens.LessOrEqual(left, right)))
    )

    opParser.AddOperator(
        InfixOperator(">", ws, 13, Associativity.None, (fun left right -> Tokens.GreaterThan(left, right)))
    )

    opParser.AddOperator(
        InfixOperator(">=", ws, 14, Associativity.None, (fun left right -> Tokens.GreaterOrEqual(left, right)))
    )


    // Set the expression parser's reference to the operator parser's expression parser
    expressionRef.Value <- opParser.ExpressionParser

    // Define a top-level parser if necessary, to handle the entire input
    let topLevelParser: Parser<Tokens.Statement list, unit> = wsn >>. statementsList .>> wsn .>> eof

    let parse input =
        match run topLevelParser input with
        | Success(res, _, _) -> Result.Ok res
        | Failure(err, _, _) -> Result.Error err
