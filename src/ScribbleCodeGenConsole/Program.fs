module Main

open System.IO
open System.Text.RegularExpressions
open ScribbleCodeGen
open Argu

type CliArgument =
    | [<Mandatory>][<MainCommand>] Filename of string
    | [<Mandatory>] Protocol of string
    | [<Mandatory>] Role of string
    | [<AltCommandLine("--legacy")>] Legacy_Api
    | [<AltCommandLine("-o")>] Output of string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Filename _ -> "Path to Scribble Output"
            | Protocol _ -> "Name of Scribble Protocol"
            | Role _ -> "Name of Local Role in the Protocol"
            | Legacy_Api -> "Use Legacy (Non-Event) Style API"
            | Output _ -> "Path to Output Filename"

let fixQuotes stuff =
    (* DotParser has issues parsing escaped quotes, we replace them with single quotes *)
    (* This can be removed after https://github.com/auduchinok/DotParser/pull/6 is merged *)
    Regex.Replace(stuff, "\\\\\"", "$")

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter()
    let parser = ArgumentParser.Create<CliArgument>(errorHandler = errorHandler, programName = "ScribbleCodeGenConsole.exe")
    let results = parser.Parse()
    let filename = results.GetResult Filename
    let protocol = results.GetResult Protocol
    let localRole = results.GetResult Role
    let legacyApi = results.Contains Legacy_Api
    if results.Contains Output
    then
        let outputFileName = results.GetResult Output
        CodePrinter.fileName := outputFileName
    let content = File.ReadAllText(filename)
    let content = fixQuotes content
    Library.processScribbleOutput content protocol localRole legacyApi
    0
