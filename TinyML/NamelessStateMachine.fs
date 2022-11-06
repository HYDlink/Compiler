module TinyML.NamelessStateMachine

type Expr =
    | Cst of int
    | Add of Expr * Expr
    | Mult of Expr * Expr
    | Var of string
    | Let of string * Expr * Expr // { Name: string; Value: Expr; Environment: Expr}

type Inst =
    | Const of int
    | Addition
    | Multiply
    | Variable of int
    | Swap
    | Pop

type Env = (string * int) list

module Env =
    let get name env =
        env
        |> List.find (fun e -> fst e |> (=) name)
        |> snd


let rec eval expr env =
    match expr with
    | Cst i -> i
    | Add (l, r) -> (eval l env) + (eval r env)
    | Mult (l, r) -> (eval l env) * (eval r env)
    | Var (name) -> Env.get name env
    | Let (name, valueExpr, scope) ->
        let value = (eval valueExpr env)
        let newEnv = (name, value) :: env
        (eval scope newEnv)

/// StackIndex: store stack index (from stack bottom to top) at let provider
/// curIndex ... 2; 1;   ...
///          ... x  y    ...
/// get variable index from current stack -> curIndex - stackIndex
type NamelessEnv = { Name: string; StackIndex: int }

/// To Stack Machine Instruction
/// [[ ]] means, modify stack only to push one element - Stack Balanced principle
/// e ↓ i
///
/// [[ Cst i ]] -> Const i
/// [[ Add(e1, e2) ]] -> [[ e1 ]] ; [[ e2 ]]; Addition
/// [[ Mul(e1, e2) ]] -> [[ e1 ]] ; [[ e2 ]]; Multiply
/// [[ Let(x, ev, es) ]] -> [[ ev ]] ; [[ es ]] ; Swap ; Pop
///
/// curSI: current Stack Index, current stack item size, top item to input
let rec eval2inst expr curSI (env: NamelessEnv list) =
    let nextSI = curSI + 0
    let sndSI = curSI + 1

    match expr with
    | Cst i -> [ Const i ]
    | Add (l, r) ->
        (eval2inst l nextSI env)
        @ (eval2inst r sndSI env) @ [ Addition ]
    | Mult (l, r) ->
        (eval2inst l nextSI env)
        @ (eval2inst r sndSI env) @ [ Multiply ]
    | Var name ->
        let v =
            List.tryFind (fun item -> item.Name = name) env

        match v with
        | Some { StackIndex = si } ->
            // Variable Instruction poped, no stack left
            let curIndex = nextSI - si - 1
            printfn $"Variable {name}, {nextSI} - {si}"
            [ Variable curIndex ]
        | None -> failwith "Variable name not found"
    | Let (name, subexpr, value) ->
        let nextEnv =
            { Name = name; StackIndex = nextSI } :: env

        (eval2inst subexpr nextSI env)
        @ (eval2inst value sndSI nextEnv) @ [ Swap; Pop ]

let eval2instDirect expr = eval2inst expr 0 []

/// Cst i ↓ i
/// Add e1 e2 ↓ e1 + e2
/// Mult e1 e2 ↓ e1 * e2
///
let rec evalStack (instructions: Inst list) stack =
    match instructions with
    | [] -> stack
    | inst :: restInst ->
        match inst with
        | Const i -> evalStack restInst (i :: stack)
        | Addition ->
            match stack with
            | l :: r :: rest -> evalStack restInst ((l + r) :: rest)
            | _ -> failwithf "Addition no enough values in stack"
        | Multiply ->
            match stack with
            | l :: r :: rest -> evalStack restInst ((l * r) :: rest)
            | _ -> failwithf "Multiply no enough values in stack"
        | Variable index -> evalStack restInst (stack[index] :: stack)
        | Swap ->
            match stack with
            | l :: r :: rest -> evalStack restInst (r :: l :: rest)
            | _ -> failwithf "Swap no enough values in stack"
        | Pop ->
            match stack with
            | _ :: rest -> evalStack restInst rest
            | _ -> failwithf "Swap no enough values in stack"

let e1 =
    Add(Let("x", Add(Cst(3), Cst(4)), Mult(Var("x"), Cst(5))), Cst(1))

let e2 =
    Add(Let("x", Add(Cst(3), Cst(4)), Mult(Add(Cst(3), Var("x")), Cst(5))), Cst(1))

let test expr =
    eval expr [] |> printfn "%A"
    let i = eval2instDirect expr
    i |> printfn "%A"
    let s = evalStack i []
    s |> printfn "%A"
