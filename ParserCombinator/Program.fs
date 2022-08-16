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

let pchar ch str =
    if String.IsNullOrEmpty(str) then
        Failure "No more input"
    else if str.[0] = ch then
        let remaining = str.[1..]
        Success(ch, remaining)
    else
        Failure str

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

let pa = parseChar 'a'
let pb = parseChar 'b'
let pab = pa .>>. pb
let porab = anyOf [ 'a'; 'b' ]
let lowerCase = anyOf [ 'a' .. 'z' ]

(run pab "aba").ToString() |> printfn "%s"
(run porab "aba").ToString() |> printfn "%s"
(run porab "bba").ToString() |> printfn "%s"

(run parseThreeDigits "123A").ToString()
|> printfn "%s"
