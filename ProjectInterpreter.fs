module ProjectInterpreter

open Parser
open ProjectParser
let access (o: Object) =
    match o with
    | Main str -> str
    | Compare str -> str

let eval (e: Query) (database: Map<string, float>) : float = 
    let (head,obj,cat,comp) = e
    match cat with
    | FitIn s -> 
        let main = access obj
        printfn "%s" main
        let comp = access comp
        printfn "%s" comp
        database.[comp]/database.[main]     
    | _ -> failwith "Can't compute."

let printanswer (e: Query) (ans: float) : string =
    let (head,obj,cat,comp) = e
    "Answer: " + (ans |> string) + " " + access obj

