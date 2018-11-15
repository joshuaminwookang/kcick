module ProjectParser

open Parser

type Header = 
| HowMany of string

type Object = 
| Main of string
| Compare of string

type Category = 
| FitIn of string

type Query =  Header * Object * Category * Object

let paux = pright pws1 (pstr "can" <|> pstr "does")
let particle = pstr "an" <|> pstr "a" <|> pstr "the" <!> "article"

let is_valid(c: char)  = is_letter(c) || is_digit(c) 

let pch = psat is_valid

let pheader = pstr ("How many") |>> (fun (a) -> HowMany (a)) <!> "header"

let pobj = pright pws1 (pleft (pmany1 pch) paux)|>> (fun a -> Main (stringify a)) <!> "main object"

let pcat = pright pws1 (pstr ("fit in") |>> (fun (a) -> FitIn (a))) <!> "category"

//let pcomp =  pright pws1 (pbetween particle (pstr "?") (pright pws1 (pmany1 pch))) |>> (fun (a) -> Compare (stringify a)) <!> "comparing"


let compws = pws1 <!> "pcompws"
let pcomp = pright compws (pbetween particle (pchar '?') (pright pws1 (pstr "hhhh"))) <!> "Compare"

//let pcomp =  pright pws1 (pbetween particle (pstr "?") (pright pws1 (pmany1 pch))) |>> (fun (a) -> Compare (stringify a)) <!> "comparing"


let query = pseq pheader (pseq pobj (pseq pcat pcomp  (fun (a,b)-> (a,b))) (fun (a,(b,c)) -> (a,b,c))) (fun (a,(b,c,d)) -> (a,b,c,d))

//let query1 = pheader |>> (fun (a) -> (a))

/// <summary>Parses an entire question, including EOF.</summary>
/// <returns>A Question value.</returns>
let grammar =  pleft query peof

let parse s = 
    match (grammar (prepare s)) with
    | Success(e,_) -> Some e
    | Failure -> None
    
 