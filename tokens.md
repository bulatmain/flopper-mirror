## Токены
```
real x = 5;
```
```
-> {TYPE_REAL, ID('x'), ASSIGN, REAL_LITERAL('5')}
```
Список:
```
- STATEMENT
    - DECLARATION(<id>, <expr>)
        - DECL_REAL         "real <id> = <expr: real>"
        - DECL_STRING       "string <id> = <expr: string>"
        - DECL_BOOL         "bool <id> = <expr: bool>"
        - DECL_FUNC         "func <id> = <expr: func>"
    - ARGUMENT(<id>)
        - ARGUMENT_REAL          "real <id>"
        - ARGUMENT_STRING        "string <id>"
        - ARGUMENT_BOOL          "bool <id>"
        - ARGUMENT_FUNC          "func <id>"
    - EXPRESSION
        - LAMBDA(<args: list<arg>>, <statements: list<statement>>)            "lambda(<decl1>, ..., <decln>) <statements>"
        - LITERAL('<string>')
            - LITERAL_REAL      "([1-9]\d*)([.])?\d+"
            - LITERAL_STRING     "\"..*\""
            - LITERAL_BOOL      "true|false"
        - ID('<string>')        "[a-zA-Z_][a-zA-Z0-9_]*"
        - CALL(<id>, <exprs: list<expr>>)        "<id>(<expr1>, ..., <exprn>)"
        - OPERATOR(<exprs: list<expr>>)
            - ASSIGN                "<id> = <expr>"
            - ARITHMETIC
                - PLUS              "<expr1: real> + <expr2: real>"
                - MINUS             "<expr1: real> - <expr2: real>"
                - MULTIPLY          "<expr1: real> * <expr2: real>"
                - DIVIDE            "<expr1: real> \/ <expr2: real>"
                - INT_DIVIDE        "<expr1: real> \/\/ <expr2: real>"
            - COMPARATION
                - EQ                "<expr1> == <expr2>"
                - NEQ               "<expr1> != <expr2>"
                - GT                "<expr1: real> > <expr2: real>"
                - GTEQ              "<expr1: real> >= <expr2: real>"
                - LS                "<expr1: real> < <expr2: real>"
                - LSEQ              "<expr1: real> <= <expr2: real>"
            - LOGIC
                - AND               "<expr1: bool> & <expr2: bool>"
                - OR                "<expr1: bool> | <expr2: bool>"
                - NOT               "! <expr: bool>"
    - IFELSE           "if (<cond>) <expr> else <expr>"
            IFELSE(<expr: bool>, <then-statements: list<statement>>, <else--statements: list<statement>>)
```

Пример:

```
real m = read_real();
string str = read_string();

func factorial = lambda (real n) { 
        if (n == 1 | n == 0) { 
            1;
        } else {
            n * factorial(n - 1);
        }
    };

print(str, factorial(m));
```

```
-> {
    DECLARATION(ID('m'), READ_REAL),
    DECLARATION(ID('STR'), read_string),
    DECLARATION(
        ID('factorial'), 
        LAMBDA(
            [ARG_REAL(ID('n'))], 
            IFELSE(
                OR(EQ(ID('n'), LITERAL_REAL('1')), EQ(ID('n'), LITERAL_REAL('0'))), 
                [LITERAL_REAL(1)], 
                [MULTIPLY(ID('n'), CALL(ID('factorial'), [MINUS(ID('n'), LITERAL_REAL('1'))]))]
            )
        )
    ),
    PRINT([ID('str'), CALL(ID('factorial'), [ID('m')])])
}
```