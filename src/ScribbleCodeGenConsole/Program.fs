module Main

open System.IO
open System.Text.RegularExpressions
open ScribbleCodeGen

let fixQuotes stuff =
    (* DotParser has issues parsing escaped quotes, we replace them with single quotes *)
    (* This can be removed after https://github.com/auduchinok/DotParser/pull/6 is merged *)
    Regex.Replace(stuff, "\\\\\"", "$")

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn "Please provide a file for input!"
        1
    else
        let filename = argv.[0]
        let content = File.ReadAllText(filename)
        let content = fixQuotes content
        Library.parseScribbleOutput content
        0
