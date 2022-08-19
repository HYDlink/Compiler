open System
open Microsoft.FSharp.Collections

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

type Position = { line: int; column: int }

type InputState =
    { lines: string []
      position: Position }

type ParserPosition =
    { currentLine: string
      line: int
      column: int }


/// define an initial position
let initialPos = { line = 0; column = 0 }

/// increment the column number
let incrCol (pos: Position) = { pos with column = pos.column + 1 }

/// increment the line number and set the column to 0
let incrLine (pos: Position) = { line = pos.line + 1; column = 0 }

module InputState =
    /// Create a new InputState from a string
    let fromStr str =
        if String.IsNullOrEmpty(str) then
            { lines = [||]; position = initialPos }
        else
            let separators = [| "\r\n"; "\n" |]

            let lines =
                str.Split(separators, StringSplitOptions.None)

            { lines = lines; position = initialPos }

    let getLine (state: InputState) lineNo =
        if lineNo < state.lines.Length then
            Some state.lines[lineNo]
        else
            None // "end of line"

    let getCurLine (state: InputState) = getLine state state.position.line

    let emptyChar = '\n'

    let getCurChar (state: InputState) =
        match getCurLine state with
        | Some line ->
            let column = state.position.column

            if column < line.Length then
                line[column]
            else
                emptyChar
        | None -> emptyChar

    // return the current line
    let currentLine inputState =
        let linePos = inputState.position.line

        if linePos < inputState.lines.Length then
            inputState.lines.[linePos]
        else
            "end of file"

    /// Get the next character from the input, if any
    /// else return None. Also return the updated InputState
    /// Signature: InputState -> InputState * char option
    let nextChar input =
        let linePos = input.position.line
        let colPos = input.position.column
        // three cases
        // 1) if line >= maxLine ->
        //       return EOF
        // 2) if col less than line length ->
        //       return char at colPos, increment colPos
        // 3) if col at line length ->
        //       return NewLine, increment linePos

        if linePos >= input.lines.Length then
            input, None
        else
            let currentLine = currentLine input

            if colPos < currentLine.Length then
                let char = currentLine.[colPos]
                let newPos = incrCol input.position

                let newState =
                    { input with position = newPos }

                newState, Some char
            else
                // end of line, so return LF and move to next line
                let char = '\n'
                let newPos = incrLine input.position

                let newState =
                    { input with position = newPos }

                newState, Some char

let rec readAllChars input =
    [ let remainingInput, charOpt =
          InputState.nextChar input

      match charOpt with
      | None ->
          // end of input
          ()
      | Some ch ->
          // return first character
          yield ch
          // return the remaining characters
          yield! readAllChars remainingInput ]

module ParserPosition =
    let fromInputState (input: InputState) =
        { currentLine = InputState.currentLine input
          line = input.position.line
          column = input.position.column }

type ParserLabel = string
type ParserError = string
// 'of' means of type
type ParseResult<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserPosition * ParserError

let printResult (result: ParseResult<'a>) =
    match result with
    | Success (a, _) -> $"%A{a}"
    | Failure (label, pos, error) ->
        let msg =
            $"Line:{pos.line}, Col:{pos.column} Error Parsing {label}"

        let line = pos.currentLine

        let hint =
            sprintf "%*s^%s" pos.column "" error

        $"{msg}\n{line}\n{hint}"

// how to put label into Failure result
type Parser<'a> =
    { parserFn: (InputState -> ParseResult<'a * InputState>)
      label: ParserLabel }

module Parser =
    let defaultLabel = ""

    let fromParserFn parserFn =
        { parserFn = parserFn
          label = defaultLabel }

let runInputState (parser: Parser<'a>) input = parser.parserFn input

let run (parser: Parser<'a>) input =
    InputState.fromStr input |> runInputState parser

let getLabel parser = parser.label

let setLabel parser newLabel =
    let innerParser input =
        match runInputState parser input with
        | Success a -> Success a
        | Failure (label, parserPosition, remaining) -> Failure(newLabel, parserPosition, remaining)

    { parserFn = innerParser
      label = newLabel }

let (<?>) = setLabel

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
        | Success (a, remaining) -> runInputState (f a) remaining
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

let rec parseCountFn (count:int) (parser: Parser<'a>) (input: InputState) =
    if count <= 0 then
        Success ([], input)
    else
        let thisResult = runInputState parser input
        match thisResult with
        | Success (thisValue, nextInput) ->
            let laterResult = parseCountFn (count - 1) parser nextInput
            match laterResult with
            | Success (laterValue, laterInput) ->
                Success (thisValue :: laterValue, laterInput)
            | Failure (a, b, c) -> Failure (a, b, c)
        | Failure (a, b, c) -> Failure (a, b, c)

let parseCount count parser =
    parseCountFn count parser
    |> Parser.fromParserFn
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

let pa = parseChar 'a'
let pb = parseChar 'b'
let pab = pa .>>. pb
let porab = anyOf [ 'a'; 'b' ]
let lowerCase = anyOf [ 'a' .. 'z' ]

let tryParser parser input =
    run parser input
    |> printResult
    |> Console.WriteLine

let TryParseBasicCombinator () =
    tryParser pa "aba"
    tryParser pab "aba"
    tryParser porab "aba"
    tryParser porab "bba"
    tryParser parseThreeDigits "123A"

let parseIf = parseString "if"

// tryParser parseIf "ifthen"

let manyLower = many lowerCase
let many1Lower = many1 lowerCase

let TryParseMany () =
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
    pl .>>. pr |>> fst

// only accept right parser result, ignore left
let (>>.) pl pr =
    // snd == fun (l, r) -> r
    pl .>>. pr |>> snd

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


tryParser parseAB_CD "AB \t\nCD" // Success (("AB", "CD"), "")

/// start Json Parser

type JValue =
    | JString of string
    | JNumber of double
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list

/// runs parser p, but return x rather than result
let (>>%) p x = p |>> (fun _ -> x)

let pJNull =
    parseString "null" >>% JNull <?> "null"

let pJBool =
    (parseString "true" >>% JBool true)
    .<>. (parseString "false" >>% JBool false)
    <?> "JBool"

let pJNumber =
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

    let toFloat (((integer: int option), (dec: double option)), (sci: double option)) =
        let intD =
            match integer with
            | Some i -> i |> double
            | None -> 0.

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

        JNumber result

    opt parseInt .>>. opt pDecimal .>>. opt pScience
    <?> "JNumber"
    |>> toFloat

let pJNumber_ = pJNumber .>> whitespaceChar

let pUnicodeChar =  
    let pHex = anyOf (['0'..'9'] @ ['a'..'f'] @ ['A'..'F'])
    
    let digitStrToUChar digitStr =
        let str = digitStr |> List.toArray |> String
        Int32.Parse(str,Globalization.NumberStyles.HexNumber)
        |> char

    parseString "\\u" >>. (parseCount 4 pHex) |>> digitStrToUChar <?> "UnicodeChar"

let pEscaped =
    let escapeMap : (char*char) list = [
        ('\"', '\"')
        ('b', '\b')
        ('f', '\f')
        ('n', '\n')
        ('r', '\r')
        ('t', '\t')
        // ('/', '/')
    ]
    let pMapEscape =
         let kvToEscape (k:char, v:char) =
             satisfy (fun ch -> ch = k) (v.ToString()) >>% v
         escapeMap |> List.map kvToEscape |> choice
    parseChar '\\' >>. pMapEscape <?> "EscapedChar"

let pUnescaped = satisfy (fun ch -> ch <> '\\' && ch <> '"') "Unescaped char"

let pJString =
    let pQuote = parseChar '"'
    let pInnerString = many (choice [pEscaped; pUnicodeChar; pUnescaped])
    let charListToString charList =
        charList |> List.toArray |> String |> JString
    pQuote >>. pInnerString .>> pQuote <?> "JString" |>> charListToString

let rec pJArray =
    let pJValue = choice [pJNull; pJBool; pJNumber; pJString ] //pJArray; pJObject]
    let pLeft = parseChar '[' .>> anyWhitespace
    let pRight = anyWhitespace >>. parseChar ']'
    let pSep = parseChar ',' .>> anyWhitespace
    
    let toJArray (input: (JValue * (JValue list)) option) =
        match input with
        | None -> JArray []
        | Some (head, tail) -> head :: tail |> JArray
        
    // ignore first object sep, but remaining object must head with sep
    let pArray = (opt (pJValue .>>. (many (pSep >>. pJValue))))
    pLeft >>. pArray .>> pRight <?> "JArray" |>> toJArray
and pJObject =
    let pJValue = choice [pJNull; pJBool; pJNumber; pJString ]// pJArray; pJObject]
    let pLeft = parseChar '{' .>> anyWhitespace
    let pRight = anyWhitespace >>. parseChar '}'
    let pSep = parseChar ',' .>> anyWhitespace
    let pColon = anyWhitespace >>. parseChar ':' .>> anyWhitespace
    
    let UnwrapJString js =
        match js with
        | JString str -> str
        | _ -> ""
    let pItem = pJString |>> UnwrapJString .>> pColon .>>. pJValue
    
    let toJObject (input: ((string * JValue) * ((string *JValue) list)) option) =
        match input with
        | None -> Map [] |> JObject
        | Some (head, tail) -> head :: tail |> Map |> JObject
        
    let pArray = (opt (pItem .>>. (many (pSep >>. pItem))))
    pLeft >>. pArray .>> pRight
    <?> "JObject"
    |>> toJObject

let TryParseJString() =
    tryParser pUnicodeChar "\\u263A"
    tryParser pJString "\"\🌀\""
    tryParser pJString "\"23423\""
    tryParser pJString "\"\\u2345\""
    tryParser pJString "\"haha\""
    tryParser pJString "\"why\\tnot\""

let TryParseJson () =
    tryParser pJBool "true"
    tryParser pJBool "false"
    tryParser pJBool "faa"
    tryParser pJNull "null"
    tryParser pJNumber "1"
    tryParser pJNumber "1342"
    tryParser pJNumber "0342"
    tryParser pJNumber "-3.42e-3"
    // too large
    // tryParser pJNumber "-237462374673276894279832749832423479823246327846"
    tryParser pJNumber ".0342"
    tryParser pJNumber ".0342e4"
    tryParser pJArray "[ 23, 24,null, true, false]"
    tryParser pJObject "{\"haha\": 23, \"NoNo\": null }"

let TryParseCount () =
    let parse4Digit = parseCount 4 parseDigit
    tryParser parse4Digit "1234"
    tryParser parse4Digit "0236"
    tryParser parse4Digit "02"
    tryParser parse4Digit ""
    tryParser parse4Digit "123465"
    tryParser parse4Digit "12365"

// TryParseCount()
// TryParseJson()
TryParseJString()
// printfn "☺"