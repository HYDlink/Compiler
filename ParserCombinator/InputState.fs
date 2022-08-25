module ParserCombinator.InputState

open System

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
