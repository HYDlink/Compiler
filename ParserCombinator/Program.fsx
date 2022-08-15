open System
printfn "Hello world"
let myInt = 5

// must declear 'rec' before recursion function
// function defintion must change line after declearation
let rec fib x = 
    match x with
    | 0 -> 0
    | 1 -> 1
    | _ -> x * fib (x-1)

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
    Success (ch, remaining)
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
            Success (ch, remaining)
        else
            Failure str
    Parser parse

let andThen pa pb =
    let func str =
        let ra = run pa str
        match ra with
            | Success (sa, remaining) -> 
                let rb = run pb remaining 
                match rb with 
                | Success (sb, result)  -> Success((sa, sb), result)
                | Failure s -> Failure s
            | Failure s -> Failure s
    Parser func
    
let ( .>>. ) = andThen
    

let orElse pa pb =
    let func str =
        let ra = run pa str
        match ra with
        | Failure sa -> run pb str
        | Success s -> Success s
    Parser func

let ( .<>. ) = orElse

let parseString str =


let pa = parseChar 'a'
let pb = parseChar 'b'
let pab = pa .>>. pb
let porab = pa .<>. pb

(run pab "aba").ToString() |> printfn "%s"
(run porab "aba").ToString() |> printfn "%s"
(run porab "bba").ToString() |> printfn "%s"