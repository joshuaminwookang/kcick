module ProjectInterpreter

open Parser
open ProjectParser

let access (o: Object) =
    match o with
    | Main str -> (str, "")
    | Compare (str, unit) -> (str, unit)

let access2 (c: Category) =
    match c with
    | FitIn str -> str
    | SoldIn str -> str


let evaluateunit (unit: string) : float = 
    match unit with 
    | " every week" | " per week"-> 7.0 
    | " every month" | " per month" -> 30.0
    | " every year" | " per year"-> 365.0
    | _ -> 1.0

let checkunit (unit: string) =
    if unit <> "" then
        printfn "Could not understand question!"
        exit 1

let eval (e: Query) (database: Map<string, Map<string, float>>) : float = 
    let (head,obj,cat,comp) = e
    let main = fst (access obj)
    //printfn "%s" main
    let (compare, unit) = access comp
    //printfn "%s" compare
    //printfn "%s" unit


    //printfn "%s" (access2 cat)
    let thisMap = database.[access2 cat]

    // check if objects are included 
    if not(thisMap.ContainsKey main) || not(thisMap.ContainsKey compare) then
        printfn "Hmmmm... I don't recognize those objects!" 
        exit 1
    match cat with
    | FitIn s -> 
        checkunit(unit)
        thisMap.[compare]/thisMap.[main]
    | SoldIn s ->  
        thisMap.[compare]*thisMap.[main]*evaluateunit(unit)
    | _ -> failwith "Can't compute."

let printanswer (e: Query) (ans: float) : string =
    let (head,obj,cat,comp) = e
    "Answer: " + (ans |> string) + " " + fst (access obj)

