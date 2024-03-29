module TinyML.LambdaMachine

type Expr = 
    | Cst of value: int
    // | Add of left: Expr * right: Expr
    // | Mult of left: Expr * right: Expr
    | Var of name: string
    | Let of var: string * def: Expr * env: Expr
    | LetRec of var: string * def: Expr * env: Expr
    | Fn of param: string * body: Expr 
    | Apply of func: Expr * arg: Expr
    | BuiltInBiOp of (int -> int -> int)
    | If of Cond: Expr * Then: Expr * Else: Expr
    
module Expr =
    /// 
    let fnParams (body: Expr) (ps: string list) =
        List.fold (fun prevExpr nextParam -> Fn(nextParam, prevExpr)) body ps
    
    let applyArgs (expr: Expr) args =
        (expr, args) ||>
        List.fold (fun prevExpr nextArg -> Apply(prevExpr, nextArg)) 

type Inst = Const of int | Addition | Multiply | Variable of int | Swap | Pop

type Primitive = Int | Bool
type Type =
    | Prim of Primitive
    | Func of Param: Type * Ret: Type
    | Generic of id: string


type Env = (string * int) list
module Env =
    let get name env =
        env |>
        List.find (fun e -> fst e |> (=) name)
        |> snd

type Value = VInt of int | VClosure of Closure
and VEnv = (string * Value) list
and Closure = { Env: VEnv; Param: string; Body: Expr}


let log msg a =
    printfn $"{msg} {a}"
    a
    
module VEnv = 
    let get name (venv: VEnv) =
        venv |>
        List.tryFind (fun e -> fst e |> (=) name)
        |> function
            | Some a -> snd a
            | None -> failwithf $"variable not found in Venv: %A{name}"
    
    let value = function 
    | VInt i -> i 
    | _ -> failwith "not int"

    let vadd v1 v2 =
        (v1 |> value)
        +
        (v2 |> value)
        |> VInt
    
    let vmul v1 v2 =
        (v1 |> value)
        *
        (v2 |> value)
        |> VInt

    let createBiOpClosure binOp =
        Fn ("_l", Fn ("_r", (BuiltInBiOp binOp)))
    
    let evaluateBiOp biOp env =
        let l = get "_l" env
        let r = get "_r" env
        biOp (l |> value) (r |> value)
        |> VInt
    
    let buildInEnv =
        [
            ("(+)", createBiOpClosure (+))
            ("(-)", createBiOpClosure (-))
            ("(*)", createBiOpClosure (*))
            ("(=)", createBiOpClosure (-))
        ]

let applyBiOp op (l, r) =
    Apply (Apply (Var op, l), r)

let Add =
    applyBiOp "(+)"
let Mult =
    applyBiOp "(*)"

let rec listType : Expr -> (Expr * Type) list =
    function
    | Cst c -> [Cst c, Prim Int]
    // | Add(left, right)
    // | Mult(left, right) -> [ left, Prim Int; right, Prim Int ]
    //                       @ (listType left) @ (listType right)
    | Var name -> [ Var name, Generic name ]
    | Let(var, expr, env) -> [ Var var, Generic var ] @ listType expr @ listType env
    | Fn(param, body) -> [ Var param, Generic param ] @ listType body
    | Apply(func, expr) -> []

let rec getFreeVars: Expr -> string Set =
    function
    | Cst i -> Set.empty
    | BuiltInBiOp func -> Set.empty
    | Var(name) -> Set [name]
    | Let(name, valueExpr, scope) ->
        Set.union
            (getFreeVars valueExpr)
            (Set.remove name (getFreeVars scope))
    | Fn (param, body) -> getFreeVars body |> Set.remove param 
    | Apply(func, arg) -> Set.union (getFreeVars func) (getFreeVars arg)
    | If(cond, ``then``, ``else``) -> 
        Set.unionMany
            [|
              (getFreeVars cond)
              (getFreeVars ``then``)
              (getFreeVars ``else``)
            |]

let evaluate (expr: Expr) =
    /// Func 需要捕获环境，将捕获了的变量，和自身的值返回出来
    let rec eval (expr: Expr) (env: VEnv) : Value = 
        match expr with
        | Cst i -> VInt i
        | BuiltInBiOp func ->
            // retrieve left value, and right value from env
            VEnv.evaluateBiOp func env
        | Var(name) ->
            VEnv.get name env
        | Let(name, valueExpr, scope) 
            -> 
            let value = (eval valueExpr env)
            let newEnv = (name, value) :: env
            (eval scope newEnv)
        | Fn (param, body) -> 
            printfn $"Func Env: {env}"
            VClosure { Env = env; Param = param; Body = body }
        | Apply(func, arg) ->
            match eval func env with
            | VClosure { Env = venv; Param = param; Body = body }
                -> let argVal = (eval arg env)
                   let newEnv = (param, argVal) :: venv
                   eval body newEnv
            | _ -> failwith ""
        | If(cond, ``then``, ``else``) ->
            match eval cond env with
            | VClosure _ -> failwith "not correct type for if"
            | VInt i ->
                if i = 0 then
                    eval ``then`` env
                else
                    eval ``else`` env
    
    let buildInEnv = List.map (fun (a, b) -> (a, eval b [])) VEnv.buildInEnv
    eval expr buildInEnv

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
let rec eval2inst expr curSI (env: NamelessEnv list)  = 
    let nextSI = curSI + 0
    let sndSI = curSI + 1
    match expr with
    | Cst i -> [ Const i ]
    | Var name -> 
        let v = List.tryFind (fun item -> item.Name = name) env
        match v with
        | Some { StackIndex = si } ->
            // Variable Instruction poped, no stack left
            let curIndex = nextSI - si - 1
            printfn $"Variable {name}, {nextSI} - {si}"
            [ Variable curIndex ]
        | None -> failwith "Variable name not found"
    | Let(name, subexpr, value) -> 
        let nextEnv = { Name = name; StackIndex = nextSI } :: env
        (eval2inst subexpr nextSI env)
        @
        (eval2inst value sndSI nextEnv)
        @ [ Swap; Pop ]

let eval2instDirect expr = eval2inst expr 0 []

/// Cst i ↓ i
/// Add e1 e2 ↓ e1 + e2
/// Mult e1 e2 ↓ e1 * e2
/// 
let rec evalStack (insts: Inst list) stack = 
    if List.isEmpty insts then
        stack
    else
        let inst :: restInst = insts
        match inst with
        | Const i -> 
            evalStack restInst (i :: stack)
        | Addition -> 
            match stack with
            | l :: r :: rest -> 
                evalStack restInst ((l + r) :: rest)
            | _ -> failwithf "Addition no enough values in stack"
        | Multiply -> 
            match stack with
            | l :: r :: rest -> 
                evalStack restInst ((l * r) :: rest)
            | _ -> failwithf "Multiply no enough values in stack"
        | Variable index ->
            evalStack restInst (stack[index] :: stack)
        | Swap -> 
            match stack with
            | l :: r :: rest ->
                evalStack restInst (r :: l :: rest)
            | _ -> failwithf "Swap no enough values in stack"
        | Pop -> 
            match stack with
            | _ :: rest ->
                evalStack restInst rest
            | _ -> failwithf "Swap no enough values in stack"

let e1 = 
    Add(
        Let("x", 
            Add(Cst(3), Cst(4)), 
            Mult(Var("x"), Cst(5))
            ),
        Cst(1)
    )

let e2 = 
    Add(
        Let("x", 
            Add(Cst(3), Cst(4)), 
            Mult(
                Add(Cst(3), Var("x"))
                , Cst(5))
            ),
        Cst(1)
    )

let ef1 =
    Let("f",
        Let("z", Cst(4),
            Expr.fnParams (Mult(Add((Var "a"), (Var "b")), (Var "z"))) ["a"; "b"]
            ),
        Let("x",
            Cst(2),
            Let("y",
                Cst(3),
                Expr.applyArgs (Var "f") [(Var "x"); (Var "y")]
            )
        )
    )

let ef2 = Let ("add2", (Expr.fnParams (Add (Var "x", Mult (Var "y", Cst 2))) ["x"; "y"]),
   Let ("z", Cst 3, Expr.applyArgs (Var "add2") [Cst 1; Var "z"]))

let test expr = 
    evaluate expr |> printfn "%A"
    let i = eval2instDirect expr
    i |> printfn "%A"
    let s = evalStack i []
    s |> printfn "%A"

let testFunc expr =
    evaluate expr |> printfn "%A"

let tester() =
    testFunc ef1
