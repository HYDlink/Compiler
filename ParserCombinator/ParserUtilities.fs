module ParserCombinator.ParserUtilities

open System
open Microsoft.FSharp.Collections
open ParserCombinator.Parser
open ParserCombinator.ParserCombinator
let lowerCase = anyOf [ 'a' .. 'z' ]
let manyLower = many lowerCase
let many1Lower = many1 lowerCase

let parseInt =
    let resultToInt (sign, digitList) =
        let i =
            digitList |> List.toArray |> System.String |> int

        match sign with
        | Some _ -> -i
        | None -> i

    let parseSign = parseChar '-' |> opt

    parseSign .>>. (many1 parseDigit) <?> "int"
    |>> resultToInt
    
let tryParser parser input =
    run parser input
    |> printResult
    |> Console.WriteLine


let pa = parseChar 'a'
let pb = parseChar 'b'
let pab = pa .>>. pb
let porab = anyOf [ 'a'; 'b' ]


let TryParseBasicCombinator () =
    tryParser pa "aba"
    tryParser pab "aba"
    tryParser porab "aba"
    tryParser porab "bba"
    tryParser parseThreeDigits "123A"

let parseIf = parseString "if"

// tryParser parseIf "ifthen"


let TryParseMany () =
    tryParser manyLower "abaB"
    tryParser many1Lower "abaB"
    tryParser many1Lower "|abaB"


let tryParseInt () =
    tryParser parseInt "1ABC" // Success (1, "ABC")
    tryParser parseInt "12BC" // Success (12, "BC")
    tryParser parseInt "123C" // Success (123, "C")
    tryParser parseInt "1234" // Success (1234, "")
    tryParser parseInt "-1234" // Success (-1234, "")
    tryParser parseInt "ABC" // Failure "Expecting '9'. Got 'A'"

let digit = anyOf [ '0' .. '9' ]

let whitespaceChar =
    anyOf [ ' '; '\t'; '\n'; '\r' ]

let whitespace = many1 whitespaceChar

let anyWhitespace = many whitespaceChar

let parseAB_CD =
    parseString "AB" .>> whitespace
    .>>. parseString "CD"

/// Keep only the result of the middle parser
let between p1 p2 p3 = p1 >>. p2 .>> p3


// tryParser parseAB_CD "AB \t\nCD" // Success (("AB", "CD"), "")

let pNumber =
    let pMinus = parseChar '-' <?> "Minus"
    let pZero = parseChar '0' <?> "Zero"

    let pHeadDigit =
        anyOf [ '1' .. '9' ] <?> "HeadDigit"

    let pDot = parseChar '.' <?> "Dot"

    let pInteger =
        (opt pMinus)
        .>>. (pZero .<>. pHeadDigit)
        .>>. (many parseDigit)
        <?> "Integer"

    let decimal (point, chList) =
        point :: chList
        |> List.toArray
        |> System.String
        |> double

    let pDecimal =
        pDot .>>. (many parseDigit) <?> "Decimal"
        |>> decimal

    let scienceToInt (sign: char option, charList) =
        let i =
            charList |> List.toArray |> String |> double

        match sign with
        | Some _ -> -i
        | None -> i

    let pScience =
        let pSign =
            opt (anyOf [ '+'; '-' ]) <?> "Sign"

        anyOf [ 'e'; 'E' ] >>. pSign
        .>>. (many parseDigit)
        <?> "Science"
        |>> scienceToInt

    let toFloat (((integer: int), (dec: double option)), (sci: double option)) =
        let intD = integer |> double

        let body =
            match dec with
            | Some d ->
                match intD with
                | i when i < 0 -> intD - d
                | _ -> intD + d
            | None -> intD

        let result =
            match sci with
            | Some e -> body * (10. ** e)
            | None -> body
        result

    parseInt .>>. opt pDecimal .>>. opt pScience |>> toFloat <?> "Number"