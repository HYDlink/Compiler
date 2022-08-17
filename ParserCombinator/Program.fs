open System
printfn "Hello world"
let myInt = 5

// must declear 'rec' before recursion function
// function defintion must change line after declearation
let rec fib x =
    match x with
    | 0 -> 0
    | 1 -> 1
    | _ -> x * fib (x - 1)

fib 4 |> printfn "%d"

// 'of' means of type
type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> ParseResult<'a * string>)

let run parser input =
    // unwrap parser to get inner function
    let (Parser innerFn) = parser
    // call inner function with input
    innerFn input

let parseChar ch =
    let parse str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else if str.[0] = ch then
            let remaining = str.[1..]
            Success(ch, remaining)
        else
            let msg =
                sprintf "Expecting '%c'. Got '%c'" ch str.[0]

            Failure msg

    Parser parse

let andThen pa pb =
    let func str =
        let ra = run pa str

        match ra with
        | Success (sa, remaining) ->
            let rb = run pb remaining

            match rb with
            | Success (sb, result) -> Success((sa, sb), result)
            | Failure s -> Failure s
        | Failure s -> Failure s

    Parser func

let (.>>.) = andThen


let orElse pa pb =
    let func str =
        let ra = run pa str

        match ra with
        | Failure sa -> run pb str
        | Success s -> Success s

    Parser func

let (.<>.) = orElse

let choice listOfParsers = List.reduce orElse listOfParsers

let anyOf listOfChars =
    listOfChars |> List.map parseChar |> choice

let mapP fn parser =
    let innerFn input =
        let result = run parser input

        match result with
        | Success (a, str) ->
            let fa = fn a
            Success(fa, str)
        | Failure f -> Failure f

    Parser innerFn

let (<!>) = mapP
let (|>>) x f = mapP f x

let parseDigit = anyOf [ '0' .. '9' ]

let parseThreeDigits =
    let transformTuple ((c1, c2), c3) = System.String [| c1; c2; c3 |]
    
    parseDigit .>>. parseDigit .>>. parseDigit
    |>> transformTuple

let returnP x =
  let innerFn input =
    // ignore the input and return x
    Success (x,input )
  // return the inner function
  Parser innerFn

let applyP fP xP =
  // create a Parser containing a pair (f,x)
  (fP .>>. xP)
  // map the pair by applying f to x
  |> mapP (fun (f,x) -> f x)

let ( <*> ) = applyP

let lift2 fn pa pb =
    returnP fn <*> pa <*> pb

let rec sequence parserList =
  // define the "cons" function, which is a two parameter function
  let cons head tail = head::tail

  // lift it to Parser World
  let consP = lift2 cons

  // process the list of parsers recursively
  match parserList with
  | [] ->
    returnP []
  | head::tail ->
    consP head (sequence tail)

let parseString str =
    /// Helper to create a string from a list of chars
    let charListToStr charList =
        charList |> List.toArray |> System.String
    
    str
    |> List.ofSeq
    |> List.map parseChar
    |> sequence
    |>> charListToStr

let startsWith (str:string) (prefix:string) =
  str.StartsWith(prefix)

let startsWithP =
  lift2 startsWith

let rec parseZeroOrMore parser input =
    let firstResult = run parser input
    match firstResult with
    | Success (firstValue, remaining) ->
        let (tailResult, result) = parseZeroOrMore parser remaining
        (firstValue :: tailResult, result)
    | Failure _ -> ([], input)

let parseOneOrMore parser input =
    let firstResult = run parser input
    match firstResult with
    | Success (firstValue, remaining) ->
        let (tailResult, result) = parseZeroOrMore parser remaining
        Success(firstValue :: tailResult, result)
    | Failure f -> Failure f

let many parser =
    let innerFn input =
        let result = parseZeroOrMore parser input
        Success result
    Parser innerFn

let many1 parser =
    let innerFn input =
        let result = parseOneOrMore parser input
        result
    Parser innerFn

let pa = parseChar 'a'
let pb = parseChar 'b'
let pab = pa .>>. pb
let porab = anyOf [ 'a'; 'b' ]
let lowerCase = anyOf [ 'a' .. 'z' ]

let tryParser parser input =
    (run parser input).ToString()
    |> printfn "%s"

tryParser pab "aba"
tryParser porab "aba"
tryParser porab "bba"

tryParser parseThreeDigits "123A"

let parseIf = parseString "if"

tryParser parseIf "ifthen"

let manyLower = many lowerCase
let many1Lower = many1 lowerCase

tryParser manyLower "abaB"
tryParser many1Lower "abaB"
tryParser many1Lower "|abaB"

// try p, if p failed, returns None, otherwise return Some wrapped Parser result
let opt p =
    let some = p |>> Some
    let none = returnP None
    some .<>. none

// only accept left parser result, ignore right
let (.>>) pl pr =
    // fst == fun (l, r) -> l
    pl .>>. pr
    |>> fst
    
// only accept right parser result, ignore left
let (>>.) pl pr =
    // snd == fun (l, r) -> r
    pl .>>. pr
    |>> snd

let parseInt =
    let resultToInt (sign, digitList) =
        let i = digitList |> List.toArray |> System.String |> int
        match sign with
        | Some _ -> -i
        | None -> i
    let parseSign = parseChar '-' |> opt
    parseSign .>>. (many1 parseDigit) |>> resultToInt

tryParser parseInt "1ABC"  // Success (1, "ABC")
tryParser parseInt "12BC"  // Success (12, "BC")
tryParser parseInt "123C"  // Success (123, "C")
tryParser parseInt "1234"  // Success (1234, "")
tryParser parseInt "-1234" // Success (-1234, "")
tryParser parseInt "ABC"   // Failure "Expecting '9'. Got 'A'"

let digit = anyOf ['0'..'9']

let whitespaceChar = anyOf [' '; '\t'; '\n']
let whitespace = many1 whitespaceChar
let parseAB_CD = parseString "AB" .>> whitespace .>>. parseString "CD"

/// Keep only the result of the middle parser
let between p1 p2 p3 =
    p1 >>. p2 .>> p3


tryParser parseAB_CD "AB \t\nCD"   // Success (("AB", "CD"), "")