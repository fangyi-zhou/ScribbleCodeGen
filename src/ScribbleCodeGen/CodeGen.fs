namespace ScribbleCodeGen
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

    let allRoles ((_, transitions) : CFSM) =
        let accumRoles roles _ transitions =
            let newRoles = List.map (fun (t: Transition) -> t.partner) transitions
            Set.union (Set.ofList newRoles) roles
        Map.fold accumRoles Set.empty transitions

    let allStates : CFSM -> State list = snd >> Map.toList >> List.map (fst >> int)

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

    let writeContents (writer: IndentedTextWriter) (content: Content) =
        Map.iter (writeObject writer) content

    let writeRole (writer: IndentedTextWriter) (role: Role) =
        writer.Write(sprintf "type %s = %s" role role)
        writer.WriteLine()

    let generateCode (cfsm : CFSM) protocol localRole =
        let outputFile = "ScribbleGenerated.fs" (* TODO: Remove hardcoding *)
        use fileWriter = new StreamWriter(outputFile)
        use writer = new IndentedTextWriter(fileWriter)
        writer.Write("module " + !moduleName)
        writer.WriteLine()
        writer.Write("(* This file is GENERATED, do not modify manually *)")
        writer.WriteLine()
        let init, transistions = cfsm
        let states = allStates cfsm
        let roles = allRoles cfsm
        Set.iter (writeRole writer) roles
        let content : Content = List.map (fun state -> mkStateName protocol state, newObject) states |> Map.ofList
        writer.Write(sprintf "type %sInit = %s" protocol (mkStateName protocol init))
        writer.WriteLine()
        writeContents writer content
        writer.Flush()
        ()
