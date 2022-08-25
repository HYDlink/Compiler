module ParserCombinator.Parser

open ParserCombinator.InputState

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
