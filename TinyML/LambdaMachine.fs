module TinyML.LambdaMachine

type Expr = 
    | Cst of int
    | Add of Expr * Expr
    | Mult of Expr * Expr
    | Var of string
    // { Name: string; Value: Expr; Environment: Expr}
    | Let of string * Expr * Expr 
    | Fn of string list * Expr // Parameters, Function body
    | Apply of Expr * (Expr list) // Function, Arguments

type Inst = Const of int | Addition | Multiply | Variable of int | Swap | Pop

type Env = (string * int) list
module Env =
    let get name env =
        env |>
        List.find (fun e -> fst e |> (=) name)
        |> snd

type Value = VInt of int | VClosure of Closure
and VEnv = (string * Value) list
and Closure = { Env: VEnv; Params: string list; Body: Expr}

module VEnv = 
    let get name (venv: VEnv) =
        venv |>
        List.tryFind (fun e -> fst e |> (=) name)
        |> function
            | Some a -> snd a
            | None -> failwithf $"variable not found in Venv: %A{name}"
    
    /// closure.env apply to (args at venv), 
    /// return captured all env (include closure new env @ venv)
    let applyArgs closure (args: string list) venv =
        let { Env = cenv; Params = params; Body = body } = closure
        let getArg i paramName =
            let value = get args[i] venv
            (paramName, value)
        let argValues = params |> List.mapi getArg
        // find params in venv, combine with cenv, merge body
        argValues @ cenv @ venv
    
    
    let value = function 
    | VInt i -> i 
    | _ -> failwith "not int"


let vadd v1 v2 =
    (v1 |> VEnv.value)
    +
    (v2 |> VEnv.value)
    |> VInt

let vmul v1 v2 =
    (v1 |> VEnv.value)
    *
    (v2 |> VEnv.value)
    |> VInt

let log msg a =
    printfn $"{msg} {a}"
    a

/// Func 需要捕获环境，将捕获了的变量，和自身的值返回出来
let rec eval (expr: Expr) (env: VEnv) : Value = 
    match expr with
    | Cst i -> VInt i
    | Add (l, r) -> 
        vadd (eval l env) (eval r env)
    | Mult (l, r ) ->
        vmul (eval l env) (eval r env)
    | Var(name) -> VEnv.get name env
    | Let(name, valueExpr, scope) 
        -> 
        let value = (eval valueExpr env)
        let newEnv = (name, value) :: env
        (eval scope newEnv)
    | Fn (params, body) -> 
        printfn $"Func Env: {env}"
        VClosure { Env = env; Params = params; Body = body }
    | Apply(func, args) ->
        /// if args.Count = param.Count, apply function directly
        /// else partial apply
        ///     add arguments to function param env
        let applyExprs closure (args: Expr list) venv =
            let { Env = cenv; Params = paras; Body = body } = closure
            assert (args.Length <= paras.Length)
            
            let getArg i =
                let value = eval args[i] venv
                (paras[i], value)
            printfn $"Args count: {args.Length}"
            let argValues = { 0 .. args.Length - 1 } |> Seq.map getArg |> Seq.toList
            // find params in venv, combine with cenv, merge body
            match args.Length = paras.Length with
            | true ->
                let newEnv = argValues @ cenv @ venv
                printfn $"Apply NewEnv: {newEnv}"
                eval closure.Body newEnv
            // partial apply, currying
            | false -> VClosure { closure with
                                  Env = argValues @ cenv
                                  Params = List.skip args.Length paras }
        let (VClosure closure) = eval func env
        applyExprs closure args env

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
    | Add (l, r) -> 
        (eval2inst l nextSI env)
        @
        (eval2inst r sndSI env)
        @ [ Addition ]
    | Mult (l, r ) -> 
        (eval2inst l nextSI env)
        @
        (eval2inst r sndSI env)
        @ [ Multiply ]
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
            Fn(["a"; "b"],
                Mult(Add((Var "a"), (Var "b")),
                    (Var "z"))
                )
            ),
        Let("x",
            Cst(2),
            Let("y",
                Cst(3),
                Apply((Var "f"), [(Var "x"); (Var "y")])
            )
        )
    )

let ef2 = Let  ("add2", Fn (["x"; "y"], Add (Var "x", Mult (Var "y", Cst 2))),
   Let ("z", Cst 3, Apply (Var "add2", [Cst 1; Var "z"])))

let test expr = 
    eval expr [] |> printfn "%A"
    let i = eval2instDirect expr
    i |> printfn "%A"
    let s = evalStack i []
    s |> printfn "%A"

let testFunc expr =
    eval expr [] |> printfn "%A"

let tester() =
    testFunc ef1