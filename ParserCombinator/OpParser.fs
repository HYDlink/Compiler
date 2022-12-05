module ParserCombinator.OpParser

open System
open Microsoft.FSharp.Core
open ParserCombinator.ParserUtilities

type Associativity =
    | No
    | Left
    | Right

[<Measure>]
type p

type Op =
    { Op: char
      Prec: int<p>
      Assoc: Associativity }

type Tree =
    | Node of TreeNode
    | Leaf of int

and TreeNode = { Op: Op; Left: Tree; Right: Tree }

module Tree =
    let print tree =
        let rec innerFn indent node () =
            match node with
            | Leaf l -> printfn "%*s%d" indent "" l
            | Node { Op = Op; Left = lhs; Right = rhs } ->
                printfn "%*s%c" indent "" Op.Op
                innerFn (indent + 1) lhs ()
                innerFn (indent + 1) rhs ()

        innerFn 0 tree ()
        printfn "\n"

let opDict =
    Map<char, Op>
        [ ('+',
           { Op = '+'
             Prec = 1<p>
             Assoc = Associativity.Left })
          ('-',
           { Op = '-'
             Prec = 1<p>
             Assoc = Associativity.Left })
          ('*',
           { Op = '*'
             Prec = 2<p>
             Assoc = Associativity.Left })
          ('/',
           { Op = '/'
             Prec = 2<p>
             Assoc = Associativity.Left }) ]

let parseE str =
    let rec parseExpr (expr: string) precd =
        let pNumber input = (int) expr[input] - (int) '0'

        let pOp input =
            let ch = expr[input]
            opDict[ch]

        let rec parseOp lhs pos precedence =
            if pos < expr.Length then
                let op = pOp pos
                let posAfterOp = pos + 1

                if op.Prec > precedence
                   || (op.Prec = precedence && op.Assoc = Right) then
                    let rhs_left_leaf = pNumber posAfterOp
                    let posAfterLeaf = posAfterOp + 1

                    let (rhs, res) =
                        parseOp (Leaf rhs_left_leaf) posAfterLeaf op.Prec

                    (Node { Op = op; Left = lhs; Right = rhs }, res)
                elif op.Prec < precedence
                     || (op.Prec = precedence && op.Assoc = Left) then
                    parseExpr (expr.Substring(posAfterOp)) op.Prec
                else
                    (lhs, pos)
            else
                (lhs, pos)

        let num = pNumber 0
        parseOp (Leaf num) 1 precd

    parseExpr str 0<p>

let rec parseExpr str =
    let mutable cur = 0
    let hasLeft () = cur < String.length str

    let peekOp () =
        let ch = str[cur]
        opDict[ch]

    let parseOp () =
        let res = peekOp ()
        cur <- cur + 1
        res

    let pNumber () =
        cur <- cur + 1
        (int) str[cur - 1] - (int) '0'

    let rec innerFn lhs minP =
        let mutable curTree = lhs

        while hasLeft () && peekOp().Prec > minP do
            let op = parseOp ()
            let num = pNumber ()
            let mutable rhs = Leaf num

            while hasLeft () && peekOp().Prec > op.Prec do
                rhs <- innerFn (Leaf num) op.Prec

            let newTree =
                Node { Op = op; Left = curTree; Right = rhs }

            curTree <- newTree

        curTree

    if hasLeft () then
        let num = Leaf(pNumber ())
        innerFn num 0<p>
    else
        Leaf 0

let test () =
    parseExpr "1+2" |> Tree.print
    parseExpr "1+2*3" |> Tree.print
    parseExpr "1+2+3" |> Tree.print
    parseExpr "1+2*3+4" |> Tree.print

