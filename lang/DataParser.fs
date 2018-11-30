module DataParser
open Parser
open ProjectParser

(*
  DataParser contains parsers required for parsing the database text file (data.txt)

  Outerkey indicates "which" map the data belongs to (e.g. fit in, sold in)
  Innerkey is the name of the object, the key of a certain map (e.g. oranges, pool) 
  Data is the actual float value of the object (e.g. 8 cubic inches)
*)

// pasrses a digit or a dot
let pnum = psat is_digit <|> pchar '.' //<!> "pnum"

// pasrses a char list in the form of a float, e.g. "20.14"
let pfloat = pmany1 pnum //<!> "pfloat"

// a recursive parser that parses the outer key "little by little" (allows to have a multi-word outer key)
let pkeylittle, pkeylittleImpl : PParser<'a> * PParser<'a> ref = precparser()
pkeylittleImpl := (fun p ->
             pseq pchlist p id
             <|> pseq pchlist (pkeylittle p) (fun (cl,(cl2,a)) -> (cl @ cl2), a)
             //<!> "pkeylittle"
             )

// parses the outer key of the database entry
let pouterkey = pleft ((pstr ("fit in") |>> (fun (a) -> a)) <|> (pstr ("sold in") |>> (fun (a) -> a))) pws1 //<!> "outerkey"
// parses the data part of the database entry
let pvalue = pright pws1 pfloat |>> (fun (a) -> (stringify a) |> float ) //<!> "value"
// parses the inner key of the database entry
let pinnerkey = pkeylittle pvalue  |>> (fun (cl,fl) -> (stringify cl, fl)) //<!> "innerkey"

// pasrses a line in the database file
let data = pseq pouterkey pinnerkey (fun (a,(b,c)) -> (a,b,c)) 

/// <summary>Parses an entire line in the database text file, including EOF.</summary>
/// <returns>A tuple of (outerkey, innerkey and data).</returns>
let datagrammar =  pleft data peof

/// <summary>Parses a database input.</summary>
/// <returns>A option of the tuple of (outerkey, innerkey and data).</returns>
let parseData s = 
    match (datagrammar (prepare s)) with
    | Success(e,_) -> Some e
    | Failure -> None 