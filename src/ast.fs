module qqlang.ast

type id = string

let operationArgsCount = function
    | "*" | "/"
    | "+" | "-"
    | "==" | "!="
    | ">" | ">="
    | "<" | "<=" -> 2

type expr =
    // Root node.
    | Program of expr list
    // Abstractions/Applications.
    | Abstraction of id list * expr
    | Application of expr * expr
    // Builtins.
    | Print of expr
    // Closures.
    | Closure of expr * env
    | RClosure of expr * env * id
    // Datatypes.
    | Bool of bool
    | Float of float
    | List of expr list
    | None
    // Expressions/Statements.
    | Condition of expr * expr * expr
    | Variable of id
    // Functions.
    | Definition of id * expr * expr
    | DefinitionRecursive of id * expr * expr
    // Operations.
    | PrimitiveOperation of id
    | Operation of id * int * expr list

and env = Map<id, expr>

let binaryOperation = function
    | "*" ->   function | [ Float(a); Float(b) ] -> Float(a * b)
    | "/" ->   function | [ Float(a); Float(b) ] -> Float(a / b)
    | "+" ->   function | [ Float(a); Float(b) ] -> Float(a + b)
    | "-" ->   function | [ Float(a); Float(b) ] -> Float(a - b)
    | ">" ->   function | [ Float(a); Float(b) ] -> Bool(a > b)
    | ">=" ->  function | [ Float(a); Float(b) ] -> Bool(a >= b)
    | "<" ->   function | [ Float(a); Float(b) ] -> Bool(a < b)
    | "<=" ->  function | [ Float(a); Float(b) ] -> Bool(a <= b)
    | "==" ->  function | [ Float(a); Float(b) ] -> Bool(a = b)
    | "!=" ->  function | [ Float(a); Float(b) ] -> Bool(a <> b)

let rec eval expr env =
    match expr with
    // Root node.
    | Program expList -> match expList |> List.map (fun x -> eval x env) with
                         | [] -> None
                         | exprList -> List.last exprList
    // Abstractions/Applications.
    | Abstraction _ -> Closure(expr, env)
    | Application(expr1, expr2) -> apply (eval expr1 env) (eval expr2 env)
    // Builtins.
    | Print x -> match (eval x env) with
                 | Bool n  -> printfn $"%A{n}"; None
                 | Float n -> printfn $"%A{n}"; None
                 | List n  -> printfn $"%A{n}"; None
    // Closures.
    | Closure(exp, _) -> exp
    // Datatypes.
    | Bool n -> Bool n
    | Float n -> Float n
    | List n -> List(List.map (fun x -> eval x env) n)
    // Expressions/Statements.
    | Condition(condExpr, trueExpr, falseExpr) -> match (eval condExpr env) with
                                                  | Bool true -> eval trueExpr env
                                                  | Bool false -> eval falseExpr env
    | Variable x -> Map.find x env
    // Functions.
    | Definition(id, expr1, expr2) -> let res = (eval expr1 env) in (eval expr2 (Map.add id res env))
    | DefinitionRecursive(id, expr1, expr2) -> eval expr2 (Map.add id (RClosure(expr1, env, id)) env)
    // Operations.
    | PrimitiveOperation(pOp) -> Operation(pOp, operationArgsCount pOp, [])
    | Operation(id, n, exprList) -> Operation(id, n, exprList)

and apply expr1 expr2 =
    match expr1 with
    // Closures.
    | Closure(Abstraction(args, body), env) ->
        match expr2 with
        | List(argsList) ->
            let env' = List.zip args argsList |> Map.ofList
            let env'' = Map.fold (fun acc key arg -> Map.add key arg acc) env env'
            eval body env''
    | RClosure(Abstraction(param, body), env, id) ->
        match expr2 with
        | List(argsList) ->
            let env' = List.zip param argsList |> Map.ofList
            let env'' = Map.fold (fun acc key arg -> Map.add key arg acc) env env'
            eval body (Map.add id expr1 env'')
    // Operations.
    | Operation(id, n, args) ->
        let newArgs = args @ [expr2]
        if n = 1 then (binaryOperation id) newArgs
                 else Operation(id, n - 1, newArgs)