namespace ScribbleCodeGen
open System.IO
open System.IO
open System.CodeDom.Compiler

module CodeGen =

    type Method = string
    type Member = string

    type Object = {
        methods : Method list
        members : Member list
    }

    type Content = Map<string, Object>

    let newObject = {
        methods = []
        members = []
    }

    let defaultTypeAliasMap = Map.ofList [
        "int", "int";
        "string", "string";
        "_Unit", "unit";
    ]

    let mkStateName protocol state =
        sprintf "%s_State%d" protocol state

    let isDummy (x : string) = x.StartsWith("_")

    let moduleName = ref "ScribbleGenerated"

    let indent (writer: IndentedTextWriter) =
        writer.Indent <- writer.Indent + 1

    let unindent (writer: IndentedTextWriter) =
        writer.Indent <- writer.Indent - 1

    let writeObject (writer: IndentedTextWriter) name obj =
        writer.Write(sprintf "type %s = class" name)
        writer.WriteLine()
        indent writer
        (* TODO: write other stuff *)
        writer.Write("end")
        writer.WriteLine()
        unindent writer

    let writeContentToFile init protocol (content: Content) (filename: string) =
        use writer = new IndentedTextWriter(new StreamWriter(filename))
        writer.Write("module " + !moduleName)
        writer.WriteLine()
        writer.Write("(* This file is GENERATED, do not modify manually *)")
        writer.WriteLine()
        Map.iter (writeObject writer) content
        writer.Write(sprintf "type %sInit = %s" protocol (mkStateName protocol init))
        writer.WriteLine()
        writer.Flush()

    let generateCode (cfsm : CFSM) protocol localRole =
        let init, transistions = cfsm
        let states = CFSMConversion.allStates cfsm
        let content : Content = List.map (fun state -> mkStateName protocol state, newObject) states |> Map.ofList
        let outputFile = "ScribbleGenerated.fs" (* TODO: Remove hardcoding *)
        writeContentToFile init protocol content outputFile
        ()
