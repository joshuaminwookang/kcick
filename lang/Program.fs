open System
open ProjectParser
open DataParser
open ProjectInterpreter
open Parser

let usage() = 
    printfn "Usage: dotnet run <fileName.txt>."
    exit 1

//ReadAllLines

let rec parselines (lines, database: Map<string, Map<string, float>>) : Map<string, Map<string, float>> =
    if Array.length lines = 0 then
        printfn "%A" database
        database
    else
        let line = parseData lines.[0]
        match line with
            | Some a -> 
                printfn "%A" line
                let (outer, inner, value) = a
                if not (database.ContainsKey(outer)) then //first outer key of its type
                    let innerMap = Map.empty
                    printfn "%s" outer
                    parselines (lines.[1..], database.Add (outer, innerMap.Add(inner,value))) 
                else
                    parselines (lines.[1..], database.Add (outer, database.[outer].Add (inner,value)))                       
            | None -> database

let construct(fileName: string) : Map<string, Map<string, float>> =
    let database = Map.empty<string, Map<string, float>>
    let lines = System.IO.File.ReadAllLines(fileName) 
    printfn "%A"lines
    parselines (lines, database)
        
                  

/// <summary>Reads in a Euler question from the user and returns the answer (in float).</summary>
/// <param name="argv">A string.</param>
/// <returns>Prints out a .</returns>
[<EntryPoint>]
let main argv =
    if Array.length argv <> 1 then 
        usage()
    
    let database = construct(argv.[0])
    //printfn "%A" database
    //let volumes = [("oranges", 8.0); ("pool", 150650325.21); ("big dangerous huge cannons", 10.0)] |> Map.ofList
    //let quantity_per_capita = [("hamburgers", 0.1); ("NYC", 8623000.0); ("New York City", 8623000.0)] |> Map.ofList
    //let database = [("fit in", volumes); ("sold in", quantity_per_capita)] |> Map.ofList

    printfn "Input question:"
    let question = System.Console.ReadLine()
    let arg = parse (question)
    match arg with
    | Some a -> 
        printfn "%A"  (printanswer a (eval a database))
    | None -> printfn "Invalid syntax"    
    0 


