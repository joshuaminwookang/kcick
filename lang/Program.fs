open System
open ProjectParser
open DataParser
open ProjectInterpreter
open Parser

(* 
    Welcome to KCICK!
    This language requires an input of 'dotnet run <filename.txt>'
    where an appropriate textfile should be in the same folder as this program 
*)

let usage() = 
    printfn "Usage: dotnet run <fileName.txt>."
    exit 1
               

/// <summary>Reads in a Euler question from the user and returns the answer (in float).</summary>
/// <param name="argv">A string.</param>
/// <returns>Prints out a .</returns>
[<EntryPoint>]
let main argv =
    if Array.length argv <> 1 then 
        usage()
    
    let database = construct(argv.[0])
    printfn "Input question:"
    let question = System.Console.ReadLine()
    let arg = parse (question)
    match arg with
    | Some a -> 
        printfn "%A"  (printanswer a (eval a database))
    | None -> printfn "Invalid syntax"    
    0 


    //printfn "%A" database
    //let volumes = [("oranges", 8.0); ("pool", 150650325.21); ("big dangerous huge cannons", 10.0)] |> Map.ofList
    //let quantity_per_capita = [("hamburgers", 0.1); ("NYC", 8623000.0); ("New York City", 8623000.0)] |> Map.ofList
    //let database = [("fit in", volumes); ("sold in", quantity_per_capita)] |> Map.ofList