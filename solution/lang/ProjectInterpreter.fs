module ProjectInterpreter

open ProjectParser
open DataParser

(*
  ProjectInterpreter includes methods to evaluate input queries and to initialize 
  the KCICK database (a map of maps that depend on each category of question) by 
  reading a text file. 
*)

/// <summary>Method to access the string entry of an Object data type.</summary>
/// <returns>A tuple of (string, string).</returns>
let access (o: Object) =
    match o with
    | Main str -> (str, "")
    | Compare (str, unit) -> (str, unit)

/// <summary>Method to access the string entry of a Category data type.</summary>
/// <returns>A string.</returns>
let access2 (c: Category) =
    match c with
    | FitIn str -> str
    | SoldIn str -> str
    | ThereIn str -> str

/// <summary>Evaluates the meaning of the unit attached to the comparing object.</summary>
/// <returns>A float.</returns>
let evaluateunit (unit: string) : float = 
    match unit with 
    | " every week" | " per week"-> 7.0 
    | " every month" | " per month" -> 30.0
    | " every year" | " per year"-> 365.0
    | _ -> 1.0

let printerror() =
    printfn "Invalid question syntax! Try again?"

/// <summary>Evaluates and answers a query based on the given database.</summary>
/// <returns>A float.</returns>
let eval (e: Query) (database: Map<string, Map<string, float>>) : float = 
    let (head,obj,cat,comp) = e
    let main = fst (access obj)
    let (compare, unit) = access comp
    printfn "%s" (access2 cat)
    let thisMap = database.[access2 cat]

    // check if objects are included in the database
    if not(thisMap.ContainsKey main) || not(thisMap.ContainsKey compare) then
        printfn "Hmmmm... I don't recognize those objects!" 
        -1.0
    else 
        match cat with
        | FitIn s -> 
            if unit <> "" then 
                printerror() 
                -1.0
            else thisMap.[compare]/thisMap.[main]
        | SoldIn s ->  
            thisMap.[compare]*thisMap.[main]*evaluateunit(unit)
        | ThereIn s -> 
            if unit <> "" then
                printerror()
                -1.0
            else thisMap.[compare]*thisMap.[main]
            
/// <summary>Pretty prints the answer to a question.</summary>
/// <returns>A string.</returns>
let printanswer (e: Query) (ans: float) : string =
    let (head,obj,cat,comp) = e
    "Answer: " + (ans |> string) + " " + fst (access obj)

/// <summary>Reads in all lines from a text file.</summary>
/// <returns>A string.</returns>
let getDataArray(fileName: string) =
    let lines = System.IO.File.ReadAllLines(fileName) 
    lines

/// <summary>Recursively parses database input lines.</summary>
/// <returns>A Map of string to a Map of string to float.</returns>
let rec parselines (lines, database: Map<string, Map<string, float>>) : Map<string, Map<string, float>> =
    if Array.length lines = 0 then
        //printfn "%A" database
        database
    else
        let line = parseData lines.[0]
        printfn "%A" line
        match line with
            | Some a -> 
                let (outer, inner, value) = a
                if not (database.ContainsKey(outer)) then //first outer key of its type
                    let innerMap = Map.empty
                    //printfn "%s" outer
                    parselines (lines.[1..], database.Add (outer, innerMap.Add(inner,value))) 
                else
                    parselines (lines.[1..], database.Add (outer, database.[outer].Add (inner,value)))                       
            | None -> database

/// <summary>Reads in a text file and initializes a map of maps that will serve as our database.</summary>
/// <returns>A Map of string to a Map of string to float.</returns>
let construct(fileName: string) : Map<string, Map<string, float>> =
    let database = Map.empty<string, Map<string, float>>
    let lines = System.IO.File.ReadAllLines(fileName) 
    //printfn "%A"lines
    parselines (lines, database)