module ProjectInterpreter

open Parser
open ProjectParser

let volume = [("tennis balls", 8.0); ("oranges", 7.0);("lemons", 6.0); ("Olympic-sized swimming pool", 150000000.0)] |> Map.ofList


let eval (e: Query) (database: Map<string, string>) = 
    let (h,o,c,comp) = 
    match c with
    | FitIn s -> database.[s]
    
(*
let eval (q: Question) : float = 
    let head, obj, aux, cat, art, cons = q
    match head with
    | "How many" -> 
        match cat with 
        | "fit in" -> 
        | "sold in" -> 
        | "there in" -> 
        | _ -> failwith "Sorry, I don't know how to answer that question!"

    | _ -> failwith "Sorry, I don't know how to answer that question!"
*)


