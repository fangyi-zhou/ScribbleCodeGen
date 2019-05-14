module Main

open System.IO
open System.Text.RegularExpressions
open ScribbleCodeGen

type Foo = class
    end


let fixQuotes stuff =
    (* DotParser has issues parsing escaped quotes, we replace them with single quotes *)
    (* This can be removed after https://github.com/auduchinok/DotParser/pull/6 is merged *)
    Regex.Replace(stuff, "\\\\\"", "$")

[<EntryPoint>]
let main argv =
    if argv.Length <> 3 then
        printfn "Invalid usage!"
        (* TODO Usage *)
        1
    else
        let filename = argv.[0]
        let protocol = argv.[1]
        let localRole = argv.[2]
        let content = File.ReadAllText(filename)
        let content = fixQuotes content
        Library.processScribbleOutput content protocol localRole
        0
