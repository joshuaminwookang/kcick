// Learn more about F# at http://fsharp.org

open System
open ProjectParser
open ProjectInterpreter
open Parser

let usage() = 
    printfn "Usage: dotnet run <Your question here>."
    exit 1

/// <summary>Reads in a Euler question from the user and returns the answer (in float).</summary>
/// <param name="argv">A string.</param>
/// <returns>Prints out a .</returns>
[<EntryPoint>]
let main argv =
    if Array.length argv <> 1 then 
        usage()
    let database = [("oranges", 8.0); ("pool", 100.0)] |> Map.ofList

    let arg = parse (argv.[0])
    match arg with
    | Some a -> 
        printfn "%A"  (printanswer a (eval a database))
    | None -> printfn "Invalid syntax"    
    0 


