module ProjectInterpreter

open Parser
open ProjectParser

let access (o: Object) =
    match o with
    | Main str -> str
    | Compare str -> str

let access2 (c: Category) =
    match c with
    | FitIn str -> str
    | SoldIn str -> str


let eval (e: Query) (database: Map<string, Map<string, float>>) : float = 
    let (head,obj,cat,comp) = e
    let main = access obj
    //printfn "%s" main
    let comp = access comp
   // printfn "%s" comp

    //printfn "%s" (access2 cat)
    let thisMap = database.[access2 cat]

    // check if objects are included 
    if not(thisMap.ContainsKey main) || not(thisMap.ContainsKey comp) then
        printfn "Hmmmm... I don't recognize those objects!" 
        exit 1
    match cat with
    | FitIn s ->       
        thisMap.[comp]/thisMap.[main]  
    | SoldIn s ->       
        thisMap.[comp]*thisMap.[main]  
    | _ -> failwith "Can't compute."

let printanswer (e: Query) (ans: float) : string =
    let (head,obj,cat,comp) = e
    "Answer: " + (ans |> string) + " " + access obj

