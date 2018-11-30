module DataParser

open Parser

type PParser<'a> = Parser<'a> -> Parser<char list * 'a>

let precparser() =
  let dumbparser = (fun (p: Parser<'a>) -> fun (input: Input) -> failwith "You forgot to initialize your recursive parser.")
  let r = ref dumbparser
  (fun (p: Parser<'a>) -> (fun (input: Input) -> !r p input)), r

let is_valid(c: char) = is_digit(c) 

let pnum = psat is_valid <|> pchar '.' 
let pchwithlist = (pletter <|> pdigit <|> pchar ' ' ) |>> (fun c -> [c]) 
let pfloat = pmany1 pnum

let pkeylittle, pkeylittleImpl : PParser<'a> * PParser<'a> ref = precparser()

pkeylittleImpl := (fun p ->
             pseq pchwithlist p id
             <|> pseq pchwithlist (pkeylittle p) (fun (cl,(cl2,a)) -> (cl @ cl2), a)
             )

let pouterkey = pleft ((pstr ("fit in") |>> (fun (a) -> a)) <|> (pstr ("sold in") |>> (fun (a) -> a))) pws1 

let pvalue = pright pws1 pfloat |>> (fun (a) -> (stringify a) |> float ) 
let pinnerkey = pkeylittle pvalue  |>> (fun (cl,fl) -> (stringify cl, fl)) 

let data = pseq pouterkey pinnerkey (fun (a,(b,c)) -> (a,b,c)) 

/// <summary>Parses an entire question, including EOF.</summary>
/// <returns>A Query value.</returns>
let datagrammar =  pleft data peof

let parseData s = 
    match (datagrammar (prepare s)) with
    | Success(e,_) -> Some e
    | Failure -> None 