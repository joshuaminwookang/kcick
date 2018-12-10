open System
open ProjectParser
open DataParser
open ProjectInterpreter
open Actions
open Parser

(* 
    Welcome to KCICK!
    This language requires an input of 'dotnet run <filename.txt>'
    where an appropriate textfile should be in the same folder as this program 
*)

let usage() = 
    printfn "Usage: dotnet run <fileName.txt>."
    exit 1

let printwelcome() =
    printfn "––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––"
    printfn "Welcome to the KCICK, the KCICK Consulting Interview CracKer!"
    printfn "––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––"


let rec parselines (lines, database: Map<string, Map<string, float>>) : Map<string, Map<string, float>> =
    if Array.length lines = 0 then
        database
    else
        let line = parseData lines.[0]
        match line with
            | Some a -> 
                let (outer, inner, value) = a
                if not (database.ContainsKey(outer)) then //first outer key of its type
                    let innerMap = Map.empty
                    parselines (lines.[1..], database.Add (outer, innerMap.Add(inner,value))) 
                else
                    parselines (lines.[1..], database.Add (outer, database.[outer].Add (inner,value)))                       
            | None -> database

let getDataArray(fileName: string) =
    let lines = System.IO.File.ReadAllLines(fileName) 
    lines

let construct(fileName: string) : Map<string, Map<string, float>> =
    let database = Map.empty<string, Map<string, float>>
    parselines (getDataArray(fileName), database)

let getstarted argv : string = 
     match Array.length argv with 
     | 0 -> "data.txt"
     | 1 -> argv.[0]
     | _ -> usage ()          

/// <summary>Reads in a Euler question from the user and returns the answer (in float).</summary>
/// <param name="argv">A string.</param>
/// <returns>Prints out a .</returns>
[<EntryPoint>]
let main argv =
    let filename = getstarted argv
    let database = construct(filename)

    printwelcome()

    while (true) do
        printfn "\nWhat would you like to do?\n0] View tutorial.\n1] Ask question.\n2] View database.\n3] Update database.\n4] Quit.\n"
        let action = System.Console.ReadLine()
        match action with
        | "0" | "0]" | "0)" | "View tutorial." | "view tutorial." | "View tutorial" | "view tutorial" | "view tutorial!" | "View Tutorial.!" -> 
            tutorial()
        | "1" | "1]" | "1)" | "Ask question." | "ask question." | "Ask question" | "ask question" | "ask question!" | "Ask question!" -> 
            ask(database)
        | "2" | "2]" | "2)" | "View database." | "view database." | "View database" | "view database" | "view database!" | "View database!" ->
            view(getDataArray(filename))
        | "3" | "3]" | "3)" | "Update database." | "update database." | "Update database" | "update database" | "update database!" | "Update database!" -> 
            update(filename, getDataArray(filename))
        | "4" | "4]" | "4)" | "Quit." | "quit." | "Quit" | "quit" | "quit!" | "Quit!" -> 
            quit()
        | _ -> printfn "I don't quite know what you mean... Try again?"
    0 


    //printfn "%A" database
    //let volumes = [("oranges", 8.0); ("pool", 150650325.21); ("big dangerous huge cannons", 10.0)] |> Map.ofList
    //let quantity_per_capita = [("hamburgers", 0.1); ("NYC", 8623000.0); ("New York City", 8623000.0)] |> Map.ofList
    //let database = [("fit in", volumes); ("sold in", quantity_per_capita)] |> Map.ofList