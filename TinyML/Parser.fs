module TinyML.Parser

open FParsec.CharParsers
open TinyML.LambdaMachine
open FParsec

let pConst: Parser<Expr, unit> =
    pint32 |>> Cst
    <?> "pConst"

let pExpr, pExprRef =
    createParserForwardedToRef<Expr, unit> ()
let pEvaluate, pEvaluateRef =
    createParserForwardedToRef<Expr, unit> ()
let pCalc, pCalcRef =
    createParserForwardedToRef<Expr, unit> ()
let pPriorityAdd, pPriorityAddRef =
    createParserForwardedToRef<Expr, unit> ()
let pPriorityMultiply, pPriorityMultiplyRef =
    createParserForwardedToRef<Expr, unit> ()
let pPriorityBrace, pPriorityBraceRef =
    createParserForwardedToRef<Expr, unit> ()
// priorityRef

let withSpaces p = p .>>? spaces
    
let pId = identifier (IdentifierOptions())

/// fun pattern* -> body
/// Fn(p2, Fn(p1, body))
let pFun =
    withSpaces (pstring "fun") >>. (withSpaces (pstring "->"))
    >>. many1 (withSpaces pId) .>>. pExpr
    |>> (fun (ids, body) -> Expr.fnParams body ids)

let pVar: Parser<Expr, unit> =
    pId |>> Var
    <?> "Variable"

let pApply =
    (pVar <|> pFun) .>>.? (many1 ((anyOf " \t") >>? pEvaluate))
    // args -> (Apply (Apply value, arg1), arg2)
    |>> (fun (value, args) -> List.fold (fun prev next -> Apply(prev, next)) value args)
    <?> "Apply"

let pOperator = (many1 (anyOf "!$%&*+-./<>?@^|=")) |>> System.String.Concat

let pOperatorFun =
    withSpaces pPriorityMultiply .>>.? (withSpaces pOperator) .>>. pCalc
    |>> (fun ((left, op), right) -> Expr.applyArgs (Var $"({op})") [left; right] )

// and pPriority2 =
//     pConst <|> pBrace <|> pMultiply
// and pPriority0 =
//     pAdd <|> pPriority2

let pBrace =
    withSpaces (pchar '(') >>. (withSpaces pCalc) .>> pchar ')'

pPriorityAddRef.Value <-
    choice [ pOperatorFun
             pConst
             pBrace
             pVar ]
    <?> "Priority Add"
pPriorityMultiplyRef.Value <-
    choice [ pConst
             pBrace
             pVar ]
    <?> "Priority Multiply"
pCalcRef.Value <-
    choice [ pOperatorFun
             pConst
             pBrace
             pVar ]
    <?> "Calculate"

let pLog msg parser =
    parser |>> (fun a -> log msg a)

let pOpDefName = pchar '(' .>>. pOperator .>>. pchar ')' |>> (fun ((l, op), r) -> $"({op})")

let pLet =
    withSpaces (pstring "let")
    // >>. (withSpaces pId)
    >>. ((withSpaces (pId <|> pOpDefName)) .>>. (many (withSpaces pId))
    <?> "Ids")
    
    .>>. (withSpaces (pchar '=')
    >>. pEvaluate
    |> pLog "Evaluate"
    <?> "Definition")
    
    .>>. ((withSpaces newline)
    >>. pExpr
    <?> "Later environment")
    
    |>> (fun (((name, params), definition), later) ->
        match params with
        | [] -> Let (name, definition, later)
        | _ -> Let (name, (Expr.fnParams definition params), later))
    <?> "Let"

let pIf =
    tuple3
        (withSpaces (pstring "if") >>. (withSpaces pExpr))
        (withSpaces (pstring "then") >>. (withSpaces pExpr))
        (withSpaces (pstring "else") >>. (withSpaces pExpr))
    |>> If

pEvaluateRef.Value <-
    choice [ pOperatorFun
             pConst
             pBrace
             pApply
             pVar ]
    <?> "Evaluation"

pExprRef.Value <-
    choice [ pLet
             pIf
             pOperatorFun
             pConst
             pBrace
             pApply
             pVar ]
    <?> "Expression"

let testExpr parser expr =
    match run parser expr with
    | Success(a, unit, position) ->
        printfn $"Expression: %A{a}"
        testFunc a
        printfn ""
    | Failure(s, parserError, unit) ->
        printfn $"%A{s}"

let testFreeVar expr =
    match run pExpr expr with
    | Success(a, unit, position) ->
        printfn $"Expression: %A{a}"
        getFreeVars a |> printfn "%A"
        printfn ""
    | Failure(s, parserError, unit) ->
        printfn $"%A{s}"

let pTest () =
    testExpr pCalc "2 + 3"
    testExpr pCalc "4"
    testExpr pCalc "2 + (3 * 5)"
    testExpr pLet "let x = 3
x + 4"
    testExpr pLet "let x = 3
let y = 5
x + y"
    testExpr pLet "let x = 3
let y = x
x + y"

    testExpr pExpr "let add2 x y = x + y * 2
let z = 3
add2 1 z"

    testExpr pExpr "let add2 x y = x + y * 2
let z = 3
let add2with3 = add2 z
add2with3 4
"
    testExpr pExpr "let (@) x y = x + y * 2
3 @ 4
"

let pFreeVar() =
    testFreeVar "x"
    testFreeVar "x + y"
    testFreeVar "let x = 3
let y = 4
x + y"
    testFreeVar "let z x = x + y
z 3"

let testIf() =
    testExpr pExpr "let x = 3
if x = 4 then
3 + 4
else
6 + 7
"

    

let TestSeq() =
    printfn $"%A{({ 0 .. 1 })}"
    printfn $"%A{({ 0 .. 0 })}"