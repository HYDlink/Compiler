module TinyML.StateMachine

let log a =
    printfn "%A" a
    a

type Expr =
    | Cst of int
    | Add of Expr * Expr
    | Mult of Expr * Expr

type Inst =
    | Const of int
    | Addition
    | Multiply

let rec eval =
    function
    | Cst i -> i
    | Add (l, r) -> eval (l) + eval (r)
    | Mult (l, r) -> eval (l) * eval (r)

/// To Stack Machine Instruction
/// [[ ]] means, modify stack only to push one element - Stack Balanced principle
/// e ↓ i
///
/// [[ Cst i ]] -> Const i
/// [[ Add(e1, e2) ]] -> [[ e1 ]] ; [[ e2 ]]; Addition
/// [[ Mul(e1, e2) ]] -> [[ e1 ]] ; [[ e2 ]]; Multiply
let rec eval2inst expr =
    match expr with
    | Cst i -> [ Const i ]
    | Add (l, r) -> eval2inst (l) @ eval2inst (r) @ [ Addition ]
    | Mult (l, r) -> eval2inst (l) @ eval2inst (r) @ [ Multiply ]

/// Cst i ↓ i
/// Add e1 e2 ↓ e1 + e2
/// Mult e1 e2 ↓ e1 * e2
///
let rec evalStack insts stack =
    match insts with
    | [] -> stack
    | Const i :: restInst -> evalStack restInst (i :: stack)
    | Addition :: restInst ->
        match stack with
        | l :: r :: rest -> evalStack restInst ((l + r) :: rest)
        | _ -> failwithf "Addition no enough values in stack"
    | Multiply :: restInst ->
        match stack with
        | l :: r :: rest -> evalStack restInst ((l * r) :: rest)
        | _ -> failwithf "Multiply no enough values in stack"

let test () =

    let e1 = Mult(Add(Cst(3), Cst(4)), Cst(5))

    eval e1 |> printfn "%A"
    let i = eval2inst e1
    i |> printfn "%A"
    let s = evalStack i []
    s |> printfn "%A"
