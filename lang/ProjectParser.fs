module ProjectParser

open Parser

type Header = 
| HowMany of string

type Object = 
| Main of string
| Compare of string 

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
let paux = pstr " can" <|> pstr " does" <|> pstr " are" <|> pstr "" <!> "aux"

let particle = pstr " an" <|> pstr " a" <|> pstr " the" <|> pstr "" <!> "article"

let is_valid(c: char)  = is_letter(c) || is_digit(c) 

let pch = psat is_valid
let pword = pmany1 pch
let pchlist = (pletter <|> pdigit <|> pchar ' ' ) |>> (fun c -> [c]) <!> "pch with List"

let pheader = pstr ("How many") |>> (fun (a) -> HowMany (a)) <!> "header"

let pcatlittle, pcatlittleImpl : PParser<'a> * PParser<'a> ref = precparser()

pcatlittleImpl := (fun p ->
             pseq pchlist p id
             <|> pseq pchlist (pcatlittle p) (fun (cl,(cl2,a)) -> (cl @ cl2), a)
             <!> "pcatlittle")

let pcomplittle, pcomplittleImpl : PParser<'a> * PParser<'a> ref = precparser()

pcomplittleImpl := (fun p ->
             pseq pchlist p id
             <|> pseq pchlist (pcomplittle p) (fun (cl,(cl2,a)) -> (cl @ cl2), a)
             <!> "pcomplittle")          

let pcat = pright pws1 ((pstr ("fit in") |>> (fun (a) -> FitIn (a))) <|> (pstr ("sold in") |>> (fun (a) -> SoldIn (a)))) <!> "category"

//let pcatstr = pright pws1 ((pstr "fit in") <|> (pstr "sold in"))
let pauxcat = pright paux pcat <!> "pauxcat"

let pobjcat = pright pws1 (pcatlittle pauxcat) |>> (fun (cl,cat) -> (Main(stringify cl),cat)) <!> "main object"

//let pcomp = pright pws1 (pbetween particle (pchar '?') (pright pws1 (pmany1 pch))) |>> (fun(a) -> Compare (stringify a)) <!> "Compare"

let pcomp = pright particle (pright pws1 (pcomplittle (pchar '?'))) |>> (fun (cl,_) -> Compare(stringify cl)) <!> "comparing object"

let query = pseq pheader (pseq pobjcat pcomp (fun ((a,b),c)-> (a,b,c))) (fun (a,(b,c,d)) -> (a,b,c,d))

/// <summary>Parses an entire question, including EOF.</summary>
/// <returns>A Query value.</returns>
let grammar =  pleft query peof

let parse s = 
    match (grammar (prepare s)) with
    | Success(e,_) -> Some e
    | Failure -> None 