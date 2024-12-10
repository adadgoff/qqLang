module qqlang.parser

open FParsec

open ast

let parser, parserRef : Parser<_, unit> * Parser<_, unit> ref = createParserForwardedToRef ()
let pIdLiteral = many1SatisfyL isLetter "ValueError: invalid name of definition/variable." .>> spaces |>> Variable

// Abstractions/Applications.
let pApplication =
    pIdLiteral
    .>>. (spaces >>. skipChar '(' >>. spaces >>. many parser .>> spaces .>> skipChar ')' .>> spaces)
    |>> fun (expr1, expr2) -> Application(expr1, List(expr2))

// Builtins.
let pPrintLiteral =
    pstring "print" >>. skipChar '(' >>. spaces >>. many parser .>> skipChar ')'
    |>> fun args -> Print(Program(args))

// Datatypes.
let pFloatLiteral = pfloat .>> spaces |>> Float

// Expressions/Statements.
let pConditionLiteral =
    let buildBlock keyword =
        pstring keyword >>. spaces >>. skipChar '{' >>. spaces >>. many parser .>> spaces .>> skipChar '}' .>> spaces

    let pCondition = pstring "if" >>. spaces >>. parser .>> spaces
    let pThenBlock = buildBlock "then"
    let pElseBlock = buildBlock "else"

    pCondition .>>. pThenBlock .>>. pElseBlock
    |>> fun ((cond, trueExpr), falseExpr) -> Condition(cond, Program(trueExpr), Program(falseExpr))

let pVariableLiteral =
    let pVariableName = pstring "var" >>. spaces >>. pIdLiteral
    let pVariableValue = spaces >>. skipChar '=' >>. spaces >>. parser
    let pExtraExpressions = many parser .>> spaces

    pVariableName .>>. pVariableValue .>>. pExtraExpressions
    |>> fun ((Variable(varName), varValue), exprList) -> 
        Definition(varName, varValue, Program(exprList))


// Operations.
let binaryOperations = [
    "*"; "/"
    "+"; "-"
    "=="; "!="
    ">"; "<"
    ">="; "<="
]

let binaryOperation = choice (List.map (fun op -> pstring op >>% op) binaryOperations)

let pBinaryOperation =
    let pOperand = pFloatLiteral <|> attempt pApplication <|> pIdLiteral
    pOperand .>> spaces .>>. binaryOperation .>> spaces .>>. pOperand
    |>> fun ((expr1, binOp), expr2) -> Application(Application(PrimitiveOperation(binOp), expr1), expr2)

// Functions.
let buildPDefinitionVariable keyword = pstring keyword >>. spaces >>. pIdLiteral
let pDefinitionVariable = buildPDefinitionVariable "def"
let pDefinitionRecursiveVariable = buildPDefinitionVariable "def rec"
let pDefinitionArgs = spaces .>> skipChar '(' >>. spaces >>. many pIdLiteral .>> skipChar ')' .>> spaces
let pDefinitionBody = spaces .>> skipChar '{' >>. spaces >>. many parser .>> skipChar '}' .>> spaces
let pDefinitionExpressions = many parser .>> spaces
let buildDefinitionBuilder constructor transformArgs (((Variable(func), args), body), exprList) =
    let corArgs = args |> transformArgs
    constructor(func, Abstraction(corArgs, Program(body)), Program(exprList))
let buildDefinition = buildDefinitionBuilder Definition (List.choose (function Variable(x) -> Some x))
let buildDefinitionRecursive = buildDefinitionBuilder DefinitionRecursive (List.map (function Variable(x) -> x))

let pDefinitionLiteral =
    pDefinitionVariable .>>. pDefinitionArgs .>>. pDefinitionBody .>>. pDefinitionExpressions
    |>> buildDefinition

let pDefinitionRecursiveLiteral =
    pDefinitionRecursiveVariable .>>. pDefinitionArgs .>>. pDefinitionBody .>>. pDefinitionExpressions
    |>> buildDefinitionRecursive

// Root node.
let pQQLanguage = spaces >>. many parser .>> eof |>> Program

parserRef.Value <-
    choice [
        pDefinitionRecursiveLiteral
        pDefinitionLiteral
        pPrintLiteral
        pConditionLiteral
        attempt pBinaryOperation
        attempt pApplication 
        pVariableLiteral
        pFloatLiteral
        pIdLiteral
    ]
