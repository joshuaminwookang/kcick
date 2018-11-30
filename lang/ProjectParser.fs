module ProjectParser

open Parser

type Header = 
| HowMany of string

type Object = 
| Main of string
| Compare of string * string 

type Category = 
| FitIn of string
| SoldIn of string

type Query =  Header * Object * Category * Object

type PParser<'a> = Parser<'a> -> Parser<char list * 'a>

let precparser() =
  let dumbparser = (fun (p: Parser<'a>) -> fun (input: Input) -> failwith "You forgot to initialize your recursive parser.")
  let r = ref dumbparser
  (fun (p: Parser<'a>) -> (fun (input: Input) -> !r p input)), r

//let paux = pseq pws1 (pstr "can" <|> pstr "does" <|> pstr "are" <|> pstr "") id <!> "aux"
let paux = pstr " can" <|> pstr " does" <|> pstr " are" <|> pstr "" 

let particle = pstr " an" <|> pstr " a" <|> pstr " the" <|> pstr "" 

let is_valid(c: char)  = is_letter(c) || is_digit(c) 

let pch = psat is_valid
let pword = pmany1 pch
let pchlist = (pletter <|> pdigit <|> pchar ' ' ) |>> (fun c -> [c]) 

let pheader = pstr ("How many") |>> (fun (a) -> HowMany (a)) 

let pcatlittle, pcatlittleImpl : PParser<'a> * PParser<'a> ref = precparser()

pcatlittleImpl := (fun p ->
             pseq pchlist p id
             <|> pseq pchlist (pcatlittle p) (fun (cl,(cl2,a)) -> (cl @ cl2), a)
             )

let pcomplittle, pcomplittleImpl : PParser<'a> * PParser<'a> ref = precparser()

pcomplittleImpl := (fun p ->
             pseq pchlist p id
             <|> pseq pchlist (pcomplittle p) (fun (cl,(cl2,a)) -> (cl @ cl2), a)
             )          

let pcat = pright pws1 ((pstr ("fit in") |>> (fun (a) -> FitIn (a))) <|> (pstr ("sold in") |>> (fun (a) -> SoldIn (a)))) 

let pauxcat = pright paux pcat

let pobjcat = pright pws1 (pcatlittle pauxcat) |>> (fun (cl,cat) -> (Main(stringify cl),cat)) 

let punit = pleft (pstr " every day" <|> pstr " every week" <|> pstr " every month" <|> pstr " every year" <|> pstr " per day" <|> pstr " per week" <|> pstr " per month" <|> pstr " per year" <|> pstr "") (pchar '?') 

let pcomp = pright particle (pright pws1 (pcomplittle punit)) |>> (fun (cl,unit) -> Compare(stringify cl, unit)) 

let query = pseq pheader (pseq pobjcat pcomp (fun ((a,b),c)-> (a,b,c))) (fun (a,(b,c,d)) -> (a,b,c,d))

/// <summary>Parses an entire question, including EOF.</summary>
/// <returns>A Query value.</returns>
let grammar =  pleft query peof

let parse s = 
    match (grammar (prepare s)) with
    | Success(e,_) -> Some e
    | Failure -> None 