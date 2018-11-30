module Actions

open System
open ProjectParser
open DataParser
open ProjectInterpreter
open Parser

let tutorial() =
    printfn "\nSyntax:<Header> <object> <auxiliary verb> <category> <optional: article> <object> <optional: unit>?"
    printfn "Example: How many oranges can fit in a pool?"

let ask(database) =
    printfn "Input question:"
    let question = System.Console.ReadLine()
    let arg = parse (question)
    match arg with
    | Some a -> 
        let answer = eval a database
        if answer >= 0.0 then
            printfn "%A"  (printanswer a answer)
    | None -> printerror()

let rec viewHelper(lines) =
    if Array.length lines > 0 then
        printfn "%s" lines.[0]
        viewHelper(lines.[1..])
let view(lines) =
    viewHelper(lines)

let update(filename, lines) =
    printfn "Input a fact in this format: <category> <object> <value>"
    let input = System.Console.ReadLine()
    let fact = parseData input
    match fact with
        | Some a -> 
            printfn "%A" input
            printfn "%A" [|input|]
            let updatedlines = Array.append lines [|input|]
            printfn "%A" updatedlines
            System.IO.File.WriteAllLines(filename, updatedlines) 
            printfn "Input has been updated. Please restart program."
            exit 1
        | None -> printfn "Invalid input format."

let quit()=
    printfn "See ya next time!"
    exit 1  

 