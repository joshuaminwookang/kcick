module ProjectParser

open Parser

type Term = 
| Question of Term * Term * Term * Term
| Header of string
| Category of string
| Object of string
| Compare of string

let volume = [("tennis balls", 8.0); ("Olympic-sized swimming pool", 150000000.0)] |> Map.ofList

let is_valid(c: char)  = is_letter(c) || is_digit(c) || is_whitespace(c)

let pch = psat is_valid

let header = pseq (pmany1 pch) (pstr ";") (fun (a,b) -> Header (stringify a))
let object = pseq (pmany1 pch) (pstr ";") (fun (a,b) -> Object (stringify a))
let category = pright pws1 (pseq (pmany1 pch) (pstr ";") (fun (a,b)-> Category (stringify a)))
let compare =  pright (pws1) (pmany1 pch) |>> (fun a -> Compare (stringify a))

let question = pseq header (pseq object (pseq category compare  (fun (a,b)-> (a,b))) (fun (a,(b,c)) -> (a,b,c))) (fun (a,(b,c,d)) -> (a,b,c,d))

/// <summary>Parses an entire question, including EOF.</summary>
/// <returns>A Question value.</returns>
let grammar =  pleft question peof

let parse s = 
    match (grammar (prepare s)) with
    | Success(e,_) -> Some e
    | Failure -> None
    
 