module CS334

open System
open AlphaReduction
open Parser
open LambdaParser

/// <summary>Performs a beta substitution on a lambda expression.</summary>
/// <param name="v">A char.</param>
/// <param name="with_e">An Expr.</param>
/// <param name="in_e">An Expr.</param>
/// <returns>An Expr.</returns>
let rec sub (v: char) (with_e: Expr) (in_e: Expr) : Expr = 
    match in_e with 
    | Variable var -> 
        if var = v then
            with_e
        else
            in_e
    | Abstraction (var, e) -> Abstraction(var, sub v with_e e) 
    | Application (e1, e2) -> Application(sub v with_e e1, sub v with_e e2)

/// <summary>Performs at most one step of beta reduction on expression e.</summary>
/// <param name="e">An Expr.</param>
/// <returns>An Expr.</returns>
let rec betastep (e: Expr) : Expr = 
    match e with 
    | Variable v -> e
    | Abstraction (v, e') -> 
        let e'' = betastep e' 
        Abstraction (v, e'')
    | Application (e1, e2) -> 
         match e1 with
         | Variable v ->
             Application(e1, betastep e2)
         | Abstraction (e1v, e1e) ->
             sub e1v e2 e1e
         | Application (e1', e2') ->
             let red_e1 = betastep e1
             if red_e1 = e1 then
                 let red_e2 = betastep e2
                 if red_e2 = e2 then
                     e
                 else
                    Application(e1, red_e2)    
             else 
                Application(red_e1, e2)

/// <summary>Completely beta normalizes a lambda expression. </summary>
/// <param name="e">An Expr.</param>
/// <returns>An Expr.</returns>
let rec betanorm (e: Expr) : Expr =
    let alpha =  fst (alphanorm e (fv e) Map.empty)
    let beta = betastep alpha
    if beta = alpha then
        beta
    else 
        betanorm beta