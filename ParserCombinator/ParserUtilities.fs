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
