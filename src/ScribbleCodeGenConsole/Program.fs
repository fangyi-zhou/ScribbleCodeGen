module Main

open System.IO
open ScribbleCodeGen

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn "Please provide a file for input!"
        1
    else
        let filename = argv.[0]
        let content = File.ReadAllText(filename)
        Library.parseScribbleOutput content
        0
