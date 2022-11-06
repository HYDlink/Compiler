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

let withSpaces p = p .>> spaces
    
let pId = identifier (IdentifierOptions())


let pVar: Parser<Expr, unit> =
    pId |>> Var
    <?> "Variable"

let pApply =
    (pId .>>? spaces1) .>>.? (many1 (withSpaces pEvaluate))
    |>> (fun (id, args) -> Apply((Var id), args))
    <?> "Apply"

let pAdd =
    withSpaces pPriorityAdd .>>? (withSpaces (pchar '+')) .>>. pPriorityAdd |>> Add

let pMultiply =
    withSpaces pPriorityMultiply .>>? (withSpaces (pchar '*')) .>>. pPriorityMultiply |>> Mult
// and pPriority2 =
//     pConst <|> pBrace <|> pMultiply
// and pPriority0 =
//     pAdd <|> pPriority2

let pBrace =
    withSpaces (pchar '(') >>. (withSpaces pCalc) .>> pchar ')'

pPriorityAddRef.Value <-
    choice [ pMultiply
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
    choice [ pAdd
             pMultiply
             pConst
             pBrace
             pVar ]
    <?> "Calculate"

let pLet =
    withSpaces (pstring "let")
    // >>. (withSpaces pId)
    >>. (many1 (withSpaces pId))
    <?> "Ids"
    
    .>> withSpaces (pchar '=')
    .>>. pEvaluate
    <?> "Definition"
    
    .>> (withSpaces newline)
    .>>. pExpr
    <?> "Later environment"
    
    |>> (fun ((ids, definition), later) ->
        match ids with
        | [id] -> Let (id, definition, later)
        | id :: params -> Let (id, Fn(params, definition), later)
        | [] -> failwithf "should parse more than one id")
    <?> "Let"

pEvaluateRef.Value <-
    choice [ pAdd
             pMultiply
             pConst
             pBrace
             pVar ]
    <?> "Evaluation"

pExprRef.Value <-
    choice [ pLet
             pAdd
             pMultiply
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
        printfn $"%A{(s, parserError, unit)}"

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
