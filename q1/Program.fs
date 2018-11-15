// Learn more about F# at http://fsharp.org

open System
open ProjectParser
open ProjectInterpreter
open Parser

let usage() = 
    printfn "Usage: dotnet run <Question>."
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
        printfn "%A"  a
    | None -> printfn "Invalid syntax" 
     
    0 
    


