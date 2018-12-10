module ProjectParser
open Parser

(*
  ProjectParser contains parsers for parsing the user's Euler question given as a command-line input. 
*)

// ADTs corresponding to the syntax of KCICK
type Header = 
| HowMany of string

type Object = 
| Main of string // main object of the question
| Compare of string * string // object to be compared against; second string represents 'unit'

type Category = 
| FitIn of string
| SoldIn of string

type Query =  Header * Object * Category * Object

// Data type for recursive parsers we use in this specific program
type PParser<'a> = Parser<'a> -> Parser<char list * 'a>

/// <summary>A recursive parser that takes both an input and a parser as parameters .</summary>
/// <returns>A recursive parser.</returns>
let precparser() =
  let dumbparser = (fun (p: Parser<'a>) -> fun (input: Input) -> failwith "You forgot to initialize your recursive parser.")
  let r = ref dumbparser
  (fun (p: Parser<'a>) -> (fun (input: Input) -> !r p input)), r

// parses letters or digits 
let pch = pletter <|> pdigit
// parses words (that can include digits) 
let pword = pmany1 pch
// same as pch except it returns a list of char lists (used for parselittle)
let pchlist = (pletter <|> pdigit <|> pchar ' ' ) |>> (fun c -> [c]) //<!> "pch with List"

// parses the header 
let pheader = pstr ("How many") |>> (fun (a) -> HowMany (a)) //<!> "header"
// parses an auxillary verb, which isn't strictly required for the KCICK syntax 
let paux = pstr " can" <|> pstr " does" <|> pstr " are" <|> pstr "" //<!> "aux"
// parses an article, which isn't strictly required for the KCICK syntax 
let particle = pstr " an" <|> pstr " a" <|> pstr " the" <|> pstr "" //<!> "article"

// a recursive parser that parses the main object "little by little" (allows to have a multi-word main object)
let pmainlittle, pmainlittleImpl : PParser<'a> * PParser<'a> ref = precparser()

pmainlittleImpl := (fun p ->
             pseq pchlist p id
             <|> pseq pchlist (pmainlittle p) (fun (cl,(cl2,a)) -> (cl @ cl2), a)
             //<!> "pcatlittle"
             )
// a recursive parser that parses the comparing object "little by little" (allows to have a multi-word comparing object)
let pcomplittle, pcomplittleImpl : PParser<'a> * PParser<'a> ref = precparser()

pcomplittleImpl := (fun p ->
             pseq pchlist p id
             <|> pseq pchlist (pcomplittle p) (fun (cl,(cl2,a)) -> (cl @ cl2), a)
             //<!> "pcomplittle"
             )          

// pasrses the category of the question (fit in, sold in , etc)
let pcat = pright pws1 ((pstr ("fit in") |>> (fun (a) -> FitIn (a))) <|> (pstr ("sold in") |>> (fun (a) -> SoldIn (a)))) //<!> "category"

// parses the sequence of main object, aux verb and category 
let pobjauxcat = pright pws1 (pmainlittle (pright paux pcat)) |>> (fun (cl,cat) -> (Main(stringify cl),cat)) //<!> "main object"

// parses the unit of the comparing object (every day, per week, etc)
let punit = pleft (pstr " every day" <|> pstr " every week" <|> pstr " every month" <|> pstr " every year" <|> pstr " per day" <|> pstr " per week" <|> pstr " per month" <|> pstr " per year" <|> pstr "") (pchar '?') //<!> "unit"

// parses the comparing object 
let pcomp = pright particle (pright pws1 (pcomplittle punit)) |>> (fun (cl,unit) -> Compare(stringify cl, unit)) //<!> "comparing object"

// parses the entire question
let query = pseq pheader (pseq pobjauxcat pcomp (fun ((a,b),c)-> (a,b,c))) (fun (a,(b,c,d)) -> (a,b,c,d))

/// <summary>Parses an entire question, including EOF.</summary>
/// <returns>A Query value.</returns>
let grammar =  pleft query peof

/// <summary>Parses an question input, given as a string.</summary>
/// <returns>An option of Query.</returns>
let parse s = 
    match (grammar (prepare s)) with
    | Success(e,_) -> Some e
    | Failure -> None 