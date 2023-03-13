module TinyML.SimplyTypedLambda

type T =
    | Int
    | Bool
    | Arrow of T * T

type Term =
    | CstI of int
    | CstB of bool
    | Variable of v: string
    | Apply of from: Term * dst: Term
    | Abstract of param: string * pt: T * body: Term

type Env = Map<string, Term>

let idBool = Abstract("x", Bool, Variable "x")

let rec getType (term: Term) (env: Env) =
    match term with
    | CstI i -> Int
    | CstB _ -> Bool
    | Variable v ->
        Map.tryFind v env
        |> function
            | Some value -> getType value env
            | None -> failwithf "Variable not found"
    | Apply (from, dst) ->
        let dt = getType dst env

        match (getType from env) with
        | Arrow (arg, bodyType) ->
            assert (arg = dt)
            bodyType
        | _ -> failwithf "apply false"
    | Abstract (s, pt, body) ->
        let dv = Map.find s env
        let newEnv = Map.add s dv env
        Arrow(pt, getType body newEnv)

let checkType (term: Term) (env: Env) (t: T) = getType term env = t


let test () =
    let testType term = getType term Map.empty |> printfn "%A"
    testType (CstB true)
    testType (CstI 3)
    testType (Apply(idBool, (CstB true)))
    testType idBool

let rec eval (term: Term) (env: Env) =
    match term with
    | CstI i -> term
    | CstB b -> term
    | Variable v ->
        Map.tryFind v env
        |> function
            | Some value -> value
            | None -> failwithf "Variable not found"
    | Apply (from, term) ->
        let dv = eval term env

        match from with
        | Abstract (s, pt, body) ->
            let newEnv = Map.add s dv env
            eval body newEnv
        | _ -> failwithf "applying const t value"
    | Abstract (s, pt, body) -> term // TODO
