module TinyML.Parser

open FParsec.CharParsers
open TinyML.NamelessStateMachine
open FParsec

let pConst: Parser<Expr, unit> =
    pint32 |>> Cst
    <?> "pConst"

let pExpr, pExprRef =
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
    >>. (withSpaces pId)
    
    .>> withSpaces (pchar '=')
    .>>. pCalc
    
    .>> (withSpaces newline)
    .>>. pExpr
    
    |>> (fun ((a, b), c) -> Let (a, b, c))
    <?> "Let"

pExprRef.Value <-
    choice [ pLet
             pAdd
             pMultiply
             pConst
             pBrace
             pVar ]
    <?> "Expression"

let testExpr parser expr =
    match run parser expr with
    | Success(a, unit, position) -> test a
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
