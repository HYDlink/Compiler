module ParserCombinator.LuaParser

open System
open ParserCombinator
open ParserCombinator.Parser
open ParserCombinator.ParserUtilities

type CommonNode<'a> = 'a
type ParseNode =
    | Number of double
    | Identifier of string
    | Evaluate of (ParseNode * ParseNode)

let pIdentifier =
    let head = satisfy (fun ch -> Char.IsLetter(ch) || ch = '_') "head"
    let consequence = satisfy (fun ch -> Char.IsLetterOrDigit(ch) || ch = '_') "consequence"
    head .>>. (many consequence) |>> (fun (a, b) -> a ::b |> List.toArray |> String)

let pExpression =
    pNumber
    
let pEvaluate =
    pIdentifier .>> (anyWhitespace .>>. (parseChar '=') ) .>> anyWhitespace
    .>>. pExpression
    
let TryId() =
    tryParser pIdentifier "_"
    tryParser pIdentifier "a"
    tryParser pIdentifier "32"
    tryParser pIdentifier "a43b3_qz2"
    tryParser pIdentifier "a43b3_q>2"