module TinyML.TinyProlog

open System.Reflection.Metadata
open FParsec

type Func = { Name: string; Args: Term list }

and Term =
    | Func of Func
    | Atom of string
    | Variable of string

let rec genCodeByTerm: Term -> string =
    function
    | Func { Name = fnName; Args = args } ->
        let argsCode = Seq.map genCodeByTerm args |> String.concat ", "
        $"{fnName}({argsCode})"
    | Atom atom -> atom
    | Variable var -> var
let rec getVariablesByFunc func: string list =
    List.map getVariablesByTerm func.Args
    |> List.concat |> List.distinct
and getVariablesByTerm: Term -> string list =
    function
    | Func func -> getVariablesByFunc func 
    | Atom atom -> []
    | Variable var -> [var]

type Rule = { Head: Func; Body: Func list }

module Rule =
    let rec genCode rule =
        let head = genCodeByTerm (Func rule.Head)

        match rule.Body with
        | [] -> head + "."
        | body ->
            let bodyCode = Seq.map (Func >> genCodeByTerm) body |> String.concat ", "
            $"{head} := {bodyCode}."
    let getVariables rule =
        (getVariablesByFunc rule.Head) :: (List.map getVariablesByFunc rule.Body)
        |> List.concat |> List.distinct

type System = Rule list

let genCode sys =
    List.map Rule.genCode sys |> String.concat "\n"

let LastRuleSystem =
    let lastEnd =
        { Head =
            { Name = "last"
              Args =
                [ Variable "X"
                  Func
                      { Name = "cons"
                        Args = [ Variable "X"; Atom "nil" ] } ] }
          Body = List.empty }

    let lastFull =
        { Head =
            { Name = "last"
              Args =
                [ Variable "X"
                  Func
                      { Name = "cons"
                        Args = [ Variable "Y"; Variable "Xs" ] } ] }
          Body =
            [ { Name = "last"
                Args = [ Variable "X"; Variable "Xs" ] } ] }

    [ lastEnd; lastFull ]

let testGen () = genCode LastRuleSystem |> printfn "%A"

[<AutoOpen>]
module Parser =
    let pStartLower: Parser<string, unit> =
        lower .>>.? many letter |>> fun (h, s) -> System.String(h :: s |> List.toArray)

    let pStartUpper: Parser<string, unit> =
        upper .>>.? many letter |>> fun (h, s) -> System.String(h :: s |> List.toArray)

    let withSpace p = p .>> spaces
    let pToken str = withSpace (pstring str)
    let pTokenCh ch = withSpace (pchar ch)
    let pAtom = pStartLower |>> Atom
    let pVariable = pStartUpper |>> Variable

    let pFunc, pFuncRef =
        createParserForwardedToRef<Func, unit> ()
    let pTerm = choice [ pFunc |>> Func ; pAtom; pVariable ]
    pFuncRef.Value <-
        let pArgs = sepBy (withSpace pTerm) (pTokenCh ',')

        withSpace pStartLower
        .>>.? (between (pTokenCh '(') (pTokenCh ')') pArgs)
        |>> fun (name, args) -> { Name = name; Args = args }

    let pRule =
        let pBody = sepBy (withSpace pFunc) (pTokenCh ',')
        (withSpace pFunc) .>>.? opt ((pToken ":=") >>. pBody) .>> (pTokenCh '.')
            |>> fun (head, body) -> { Head = head; Body = if body.IsNone then list.Empty else body.Value }
    let pSystem = many (withSpace pRule)
    
let testParseTerm() =
    run pAtom "hello" |> printfn "%A"
    run pAtom "Why" |> printfn "%A"
    run pVariable "Why" |> printfn "%A"
    run pVariable "hello" |> printfn "%A"
    run pFunc "test(Haha)" |> printfn "%A"
    run pTerm "test(Haha)" |> printfn "%A"
    run pTerm "test" |> printfn "%A"
    run pTerm "Why" |> printfn "%A"
    let lastRule = run pSystem (genCode LastRuleSystem)
    match lastRule with
    | Success(rules, unit, position) -> rules = LastRuleSystem |> printfn "%A"
    | Failure _ -> printfn "%A" lastRule
    

[<AutoOpen>]
module Unification =
    let rec innerUnify left right: List<string * Term> =
        match left with
        | Func { Name= name; Args=args } ->
            match right with
            | Variable rv -> [(rv, left)]
            | Func func when func.Name = name
                -> List.zip args func.Args
                    |> List.map (fun (l, r) -> innerUnify l r) |> List.concat
            | _ -> failwith "Unify error"
        | Variable v ->
            match right with
            | Variable rv ->
                if v = rv then
                    []
                else
                    failwith "Variable not match"
            | _ -> [(v, right)] 
        | Atom a ->
            match right with
            | Variable rv -> [(rv, Atom a)]
            | _ -> failwith "Unify error"
    let unify left right =
        let result = innerUnify left right |> List.distinct
        // check conflict variable substitution
        let checkConflictVariable() =
            result
            |> List.countBy fst
            |> List.tryFind (snd >> (<) 1)
            |> function
                | Some (v, count) -> failwith $"conflict variable {v}, count {count}"
                | _ -> ()
            
        checkConflictVariable()
        result

let testUnify() =
    let testTerm left right =
        let (Success (fa, _, _)) = run pTerm left
        let (Success (fb, _, _)) = run pTerm right
        unify fa fb |> printfn "%A"
    testTerm "fun(a, g(X))" "fun(X, g(b))" // conflict, should catch
    testTerm "f(g(X), X)" "f(g(m), m)"
    
module RuleEngine =
    let query system predicate =
        let rec innerQuery pred =
            // find first pred by unify
            // if body is empty, return true
            // else
            //    foreach require: body
            //        get all unified by require
            //        unify later requirements
            //        if later false, use next current unification list
            //        if unified, return current
            []
        []