// Learn more about F# at http://fsharp.org

open System
open AlphaReduction
open Parser
open LambdaParser
open CS334

let usage() = 
    printfn "Usage: dotnet run <Lambda Expr>."
    exit 1

/// <summary>Turns an AST into a string for your reading pleasure.</summary>
/// <param name="e">An Expr.</param>
/// <returns>A string.</returns>
[<EntryPoint>]
let main argv =
    if Array.length argv <> 1 then 
        usage()
 
    let arg = parse (argv.[0])
    match arg with
    | Some a -> 
        printfn "%s" (lambdaprint (fst (alphanorm a (fv a) Map.empty)))
        printfn "%s" (lambdaprint (betanorm a))    
    | None -> printfn "Invalid syntax"     
    0 


