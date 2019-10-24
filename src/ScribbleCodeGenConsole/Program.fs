module Main

open System.IO
open System.Text.RegularExpressions
open ScribbleCodeGen
open Argu

type CliArgument =
    | [<Mandatory>][<MainCommand>] Filename of string
    | [<Mandatory>] Protocol of string
    | [<Mandatory>] Role of string
    | Mode of CodeGenMode
    | [<AltCommandLine("-o")>] Output of string
    | Recursion
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Filename _ -> "Path to Scribble Output"
            | Protocol _ -> "Name of Scribble Protocol"
            | Role _ -> "Name of Local Role in the Protocol"
            | Mode _ -> "Mode of Code Generation, default F# Event Style"
            | Output _ -> "Path to Output Filename"
            | Recursion -> "Allow Refinements on Recursion (Scribble dev-assrt)"

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter()
    let parser = ArgumentParser.Create<CliArgument>(errorHandler = errorHandler, programName = "ScribbleCodeGenConsole.exe")
    let results = parser.Parse()
    let filename = results.GetResult Filename
    let protocol = results.GetResult Protocol
    let localRole = results.GetResult Role
    let codeGenMode = results.GetResult(Mode, defaultValue=EventApi)
    let recursiveRefinement = results.Contains Recursion
    if results.Contains Output
    then
        let outputFileName = results.GetResult Output
        CodePrinter.fileName := outputFileName
    let content = File.ReadAllText(filename)
    Library.processScribbleOutput content protocol localRole codeGenMode recursiveRefinement
    0
