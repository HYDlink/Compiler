module ParserCombinator.JsonParser

open System
open Microsoft.FSharp.Collections
open ParserCombinator.Parser
open ParserCombinator.ParserCombinator
open ParserCombinator.ParserUtilities

type JValue =
    | JString of string
    | JNumber of double
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list

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

        JNumber result

    parseInt .>>. opt pDecimal .>>. opt pScience
    <?> "JNumber"
    |>> toFloat

let pJNumber_ = pJNumber .>> whitespaceChar <?> "JNumber"

let pUnicodeChar =
    let pHex =
        anyOf ([ '0' .. '9' ] @ [ 'a' .. 'f' ] @ [ 'A' .. 'F' ])

    let digitStrToUChar digitStr =
        let str = digitStr |> List.toArray |> String

        Int32.Parse(str, Globalization.NumberStyles.HexNumber)
        |> char

    parseString "\\u" >>. (parseCount 4 pHex)
    |>> digitStrToUChar
    <?> "UnicodeChar"

let pEscaped =
    let escapeMap: (char * char) list =
        [ ('\"', '\"')
          ('b', '\b')
          ('f', '\f')
          ('n', '\n')
          ('r', '\r')
          ('t', '\t')
          // ('/', '/')
          ]

    let pMapEscape =
        let kvToEscape (k: char, v: char) =
            satisfy (fun ch -> ch = k) (v.ToString()) >>% v

        escapeMap |> List.map kvToEscape |> choice

    parseChar '\\' >>. pMapEscape <?> "EscapedChar"

let pUnescaped =
    satisfy (fun ch -> ch <> '\\' && ch <> '"') "Unescaped char"

let pJString =
    let pQuote = parseChar '"'

    let pInnerString =
        many (
            choice [ pEscaped
                     pUnicodeChar
                     pUnescaped ]
        )

    let charListToString charList =
        charList |> List.toArray |> String |> JString

    pQuote >>. pInnerString .>> pQuote <?> "JString"
    |>> charListToString


let createParserForwardedToRef<'a> () =

    let dummyParser: Parser<'a> =
        let innerFn _ = failwith "unfixed forwarded parser"

        { parserFn = innerFn
          label = "unknown" }

    // mutable pointer to placeholder Parser
    let parserRef = ref dummyParser

    // wrapper Parser
    let innerFn input =
        // forward input to the placeholder
        // (Note: ".value" is the deferencing operator)
        runInputState parserRef.Value input

    let wrapperParser =
        { parserFn = innerFn
          label = "unknown" }

    wrapperParser, parserRef

let pJValue, pJValueRef =
    createParserForwardedToRef<JValue> ()

let pSep (parser: Parser<'a>) (sep: Parser<'b>): Parser<'a list> =
    let pSep = anyWhitespace >>. sep .>> anyWhitespace
    // ignore first object sep, but remaining object must head with sep
    let toList (input: ('a * ('a list)) option) =
        match input with
        | None -> []
        | Some (head, tail) -> head :: tail
     
    (opt (parser .>>. (many (pSep >>. parser)))) |>> toList

let pJArray =
    let pLeft = parseChar '[' .>>. anyWhitespace
    let pRight = anyWhitespace >>. parseChar ']'

    // ignore first object sep, but remaining object must head with sep
    let pArray = pSep pJValue (parseChar ',')

    pLeft >>. pArray .>> pRight <?> "JArray" |>> JArray

let pJObject =
    let pLeft = parseChar '{' .>> anyWhitespace |> pLog
    let pRight = anyWhitespace >>. parseChar '}'

    let pColon =
        anyWhitespace >>. parseChar ':' .>> anyWhitespace

    let UnwrapJString js =
        match js with
        | JString str -> str
        | _ -> ""

    let pItem =
        pJString |>> UnwrapJString .>> pColon .>>. pJValue |> pLog

    let toJObject (input: (string * JValue) list) =
        input |> Map |> JObject

    let pArray = pSep pItem (parseChar ',' |> pLog) |>> toJObject

    pLeft >>. pArray .>> pRight <?> "JObject"

pJValueRef.Value <- choice [ pJNull;pJBool;pJNumber_;pJString;pJArray;pJObject ] <?> "JValue"

let TryParseJString () =
    tryParser pUnicodeChar "\\u263A"
    tryParser pJString "\"\🌀\""
    tryParser pJString "\"\""
    tryParser pJString "\"a\""
    tryParser pJString "\"23423\""
    tryParser pJString "\"\\u2345\""
    tryParser pJString "\"haha\""
    tryParser pJString "\"why\\tnot\""

let TryParseJNumber () =
    tryParser pJNumber "1"
    tryParser pJNumber "1342"
    tryParser pJNumber "0342"
    tryParser pJNumber "-3.42e-3"
    tryParser pJNumber_ ".0342e4ab"
    tryParser pJNumber_ ".0342e4--"
    // too large
    // tryParser pJNumber "-237462374673276894279832749832423479823246327846"
    tryParser pJNumber ".0342"
    tryParser pJNumber ".0342e4"
    
let TryParseJson () =
    tryParser pJBool "true"
    tryParser pJBool "false"
    tryParser pJBool "faa"
    tryParser pJNull "null"

let TryParseCompleteJson () =
    tryParser pJArray "[ 23, 24,null, true, false]"
    tryParser pJObject "{\"haha\": 23, \"NoNo\": null, \"32\": null }"
    let example1 = """{
  "name" : "Scott",
  "isMale" : true,
  "bday" : {"year":2001, "month":12, "day":25 },
  "favouriteColors" : ["blue", "green"],
  "emptyArray" : [],
  "emptyObject" : {}
}"""
    printfn $"{example1}"
    tryParser pJValue example1

let TryParseCount () =
    let parse4Digit = parseCount 4 parseDigit
    tryParser parse4Digit "1234"
    tryParser parse4Digit "0236"
    tryParser parse4Digit "02"
    tryParser parse4Digit ""
    tryParser parse4Digit "123465"
    tryParser parse4Digit "12365"

TryParseCount()
TryParseJNumber()
TryParseJString()
printfn "☺"
TryParseJson()
TryParseCompleteJson()