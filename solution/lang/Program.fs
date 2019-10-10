open System
open ProjectParser
open DataParser
open ProjectInterpreter
open Actions
open Parser

(* 
    Welcome to KCICK!
    This language requires an input of 'dotnet run <filename.txt>'
    where an appropriate textfile should be in the same folder as this program 
*)

let usage() = 
    printfn "Usage: dotnet run <fileName.txt>."
    exit 1

let printwelcome() =
    printfn "––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––"
    printfn "  Welcome to KCICK, the KCICK Consulting Interview CracKer!"
    printfn "––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––"

let getstarted argv : string = 
     match Array.length argv with 
     | 1 -> argv.[0]
     | _ -> usage ()          

/// <summary>Reads in a Euler question from the user and returns the answer (in float).</summary>
/// <param name="argv">A string.</param>
/// <returns>Prints out a .</returns>
[<EntryPoint>]
let main argv =
    let filename = getstarted argv
    let database = construct(filename)
    //printfn "%A" database

    printwelcome()

    while (true) do
        printfn "\nWhat would you like to do?\n0] View tutorial.\n1] Ask question.\n2] View database.\n3] Update database.\n4] Quit.\n"
        let action = System.Console.ReadLine()
        match action with
        | "0" | "0]" | "0)" | "View tutorial." | "view tutorial." | "View tutorial" | "view tutorial" | "view tutorial!" | "View Tutorial.!" -> 
            tutorial()
        | "1" | "1]" | "1)" | "Ask question." | "ask question." | "Ask question" | "ask question" | "ask question!" | "Ask question!" -> 
            ask(database)
        | "2" | "2]" | "2)" | "View database." | "view database." | "View database" | "view database" | "view database!" | "View database!" ->
            view(getDataArray(filename))
        | "3" | "3]" | "3)" | "Update database." | "update database." | "Update database" | "update database" | "update database!" | "Update database!" -> 
            update(filename, getDataArray(filename))
        | "4" | "4]" | "4)" | "Quit." | "quit." | "Quit" | "quit" | "quit!" | "Quit!" -> 
            quit()
        | _ -> printfn "I don't quite know what you mean... Try again?"
    0 
