module ParserCombinator.ParserCombinator

open ParserCombinator.InputState
open ParserCombinator.Parser

let satisfy predicate label =
    let parserFn (input: InputState) =
        let (nextState, chOpt) =
            InputState.nextChar input

        match chOpt with
        | None ->
            let msg = "Reach end of input"
            Failure(label, (ParserPosition.fromInputState input), msg)
        | Some ch ->
            match (predicate ch) with
            | true -> Success(ch, nextState)
            | false ->
                let msg = $"Got '%c{ch}'"
                Failure(label, (ParserPosition.fromInputState input), msg)

    { parserFn = parserFn; label = label }

let parseChar ch = satisfy (fun c -> c = ch) $"{ch}"

let returnP x =
    let innerFn input =
        // ignore the input and return x
        Success(x, input)

    Parser.fromParserFn innerFn

let bindP f p =
    let innerFn input =
        match runInputState p input with
        | Success (a, remaining) ->
            // Console.WriteLine($"Success {(a, remaining.position)}")
            runInputState (f a) remaining
        | Failure (a, b, c) -> Failure(a, b, c)

    Parser.fromParserFn innerFn

let (>>=) p f = bindP f p

let andThen (pa: Parser<'a>) (pb: Parser<'b>) =
    pa
    >>= (fun paResult ->
        pb
        >>= (fun pbResult -> returnP (paResult, pbResult)))
    <?> $"{pa.label} andThen {pb.label}"

let (.>>.) = andThen


let orElse pa pb =
    let func str =
        let ra = runInputState pa str

        match ra with
        | Failure (a, b, c) -> runInputState pb str
        | Success s -> Success s

    Parser.fromParserFn func
    <?> $"{pa.label} orElse {pb.label}"

let (.<>.) = orElse

let choice listOfParsers = List.reduce orElse listOfParsers

let anyOf listOfChars =
    satisfy (fun ch -> listOfChars |> List.contains ch) $"anyOf %A{listOfChars}"

// ('a -> 'b) -> Parser<'a> -> Parser<'b>
let mapP (fn: ('a -> 'b)) (parser: Parser<'a>) : Parser<'b> = parser >>= (fun a -> (fn a) |> returnP)

let (<!>) = mapP
let (|>>) x f = mapP f x

let pLog parser =
    let wrapper a =
        printfn $"%A{a}"
        a
    mapP wrapper parser

let parseDigit =
    anyOf [ '0' .. '9' ] <?> "Digit"

let parseThreeDigits =
    let transformTuple ((c1, c2), c3) = System.String [| c1; c2; c3 |]

    parseDigit .>>. parseDigit .>>. parseDigit
    |>> transformTuple

let applyP fP xP =
    // create a Parser containing a pair (f,x)
    (fP .>>. xP)
    // map the pair by applying f to x
    |> mapP (fun (f, x) -> f x)

let (<*>) = applyP

let lift2 fn pa pb = returnP fn <*> pa <*> pb

let rec sequence parserList =
    // define the "cons" function, which is a two parameter function
    let cons head tail = head :: tail

    // lift it to Parser World
    let consP = lift2 cons

    // process the list of parsers recursively
    match parserList with
    | [] -> returnP []
    | head :: tail -> consP head (sequence tail)

let parseString (str: string) =
    /// Helper to create a string from a list of chars
    let charListToStr charList =
        charList |> List.toArray |> System.String

    str
    |> List.ofSeq
    |> List.map parseChar
    |> sequence
    |>> charListToStr
    <?> str

let startsWith (str: string) (prefix: string) = str.StartsWith(prefix)

let startsWithP = lift2 startsWith

let rec parseZeroOrMore parser input =
    let firstResult = runInputState parser input

    match firstResult with
    | Success (firstValue, remaining) ->
        let (tailResult, result) =
            parseZeroOrMore parser remaining

        (firstValue :: tailResult, result)
    | Failure _ -> ([], input)

let parseOneOrMore parser input =
    let firstResult = runInputState parser input

    match firstResult with
    | Success (firstValue, remaining) ->
        let (tailResult, result) =
            parseZeroOrMore parser remaining

        Success(firstValue :: tailResult, result)
    | Failure (a, b, c) -> Failure(a, b, c)

let rec parseCountFn (count: int) (parser: Parser<'a>) (input: InputState) =
    if count <= 0 then
        Success([], input)
    else
        let thisResult = runInputState parser input

        match thisResult with
        | Success (thisValue, nextInput) ->
            let laterResult =
                parseCountFn (count - 1) parser nextInput

            match laterResult with
            | Success (laterValue, laterInput) -> Success(thisValue :: laterValue, laterInput)
            | Failure (a, b, c) -> Failure(a, b, c)
        | Failure (a, b, c) -> Failure(a, b, c)

let parseCount count parser =
    parseCountFn count parser |> Parser.fromParserFn
    <?> $"{count} times {parser.label}"

let many parser =
    let innerFn input =
        let result = parseZeroOrMore parser input
        Success result

    Parser.fromParserFn innerFn

let many1 parser =
    let innerFn input =
        let result = parseOneOrMore parser input
        result

    Parser.fromParserFn innerFn

// try p, if p failed, returns None, otherwise return Some wrapped Parser result
let opt p =
    let some = p |>> Some
    let none = returnP None
    some .<>. none

// only accept left parser result, ignore right
let (.>>) pl pr =
    // fst == fun (l, r) -> l
    pl .>>. pr |>> fst

// only accept right parser result, ignore left
let (>>.) pl pr =
    // snd == fun (l, r) -> r
    pl .>>. pr |>> snd

/// runs parser p, but return x rather than result
let (>>%) p x = p |>> (fun _ -> x)