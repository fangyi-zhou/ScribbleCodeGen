namespace ScribbleCodeGen

open System.IO
open System.CodeDom.Compiler

open CodeGenCommon
open CodeGen

module CodePrinter =
    let moduleName = ref "ScribbleGenerated"
    let fileName = ref "ScribbleGenerated.fs"

    let indent (writer: IndentedTextWriter) =
        writer.Indent <- writer.Indent + 1

    let unindent (writer: IndentedTextWriter) =
        writer.Indent <- writer.Indent - 1

    let writeln (writer: IndentedTextWriter) (str: string) =
        writer.Write(str)
        writer.WriteLine()

    let writeTypeDefPreamble (writer: IndentedTextWriter) isFirst name content =
        let preamble =
            if isFirst then "type" else "and"
        fprintfn writer "%s %s%s" preamble name content

    let writeObject (writer: IndentedTextWriter) isFirst name obj =
        writeTypeDefPreamble writer isFirst name "() = class"
        indent writer
        List.iter (writeln writer) obj.members
        List.iter (writeln writer) obj.methods
        writeln writer "end"
        unindent writer

    let writeUnionCase (writer: IndentedTextWriter) (tag, fieldTypes, refinement) =
        let refinement =
            match refinement with
            | Some r -> sprintf "[<Refined(\"%s\")>] " r
            | None -> ""
        let fields =
            match fieldTypes with
            | [] -> ""
            | fields -> sprintf " of %s" (String.concat " * " (Seq.ofList fields))
        fprintfn writer "| %s%s%s" tag refinement fields

    let writeUnion (writer: IndentedTextWriter) isFirst name union =
        writeTypeDefPreamble writer isFirst name " ="
        indent writer
        List.iter (writeUnionCase writer) union
        unindent writer

    let writeRecordItem (writer: IndentedTextWriter) (field, fieldType, refinement) =
        let refinementAttribute =
            match refinement with
            | Some refinement -> sprintf "[<Refined(\"%s\")>] " refinement
            | None -> ""
        fprintfn writer "%s%s : %s" refinementAttribute field fieldType

    let writeRecord (writer: IndentedTextWriter) isFirst name record =
        if List.isEmpty record
        then
            (* F# doesn't allow empty record *)
            writeTypeDefPreamble writer isFirst name " = unit"
        else
            writeTypeDefPreamble writer isFirst name " = {"
            indent writer
            List.iter (writeRecordItem writer) record
            writeln writer "}"
            unindent writer

    let writeTypeDef (writer: IndentedTextWriter) isFirst (name, typeDef) =
        match typeDef with
        | Union u -> writeUnion writer isFirst name u
        | Object o -> writeObject writer isFirst name o
        | Record r -> writeRecord writer isFirst name r

    let writeContents (writer: IndentedTextWriter) (content: Content) =
        let content = Map.toList content
        match content with
            | [] -> ()
            | first :: rest ->
                writeTypeDef writer true first
                List.iter (writeTypeDef writer false) rest

    let generatePreamble writer moduleName protocol localRole =
        fprintfn writer "module %s%s%s" moduleName protocol localRole
        writeln writer ("(* This file is GENERATED, do not modify manually *)")
        writeln writer ("open FluidTypes.Annotations")
        writeln writer ""
        writeln writer """type Communications = {
    send_int : int -> unit;
    send_string : string -> unit;
    send_unit : unit -> unit;
    recv_int : unit -> int;
    recv_string : unit -> string;
    recv_unit : unit -> unit;
}
"""
        //writeln writer ("let send_int : int -> unit = failwith \"TODO\"")
        //writeln writer ("let send_string : string -> unit = failwith \"TODO\"")
        //writeln writer ("let send_unit : unit -> unit = failwith \"TODO\"")
        //writeln writer ("let recv_int : unit -> int = failwith \"TODO\"")
        //writeln writer ("let recv_string : unit -> string = failwith \"TODO\"")
        //writeln writer ("let recv_unit : unit -> unit = failwith \"TODO\"")
        //writeln writer ""


    let generateRunState (writer: IndentedTextWriter) (cfsm : CFSM) stateVarMap isInit state =
        let _, finalStates, transitions = cfsm
        let functionName = sprintf "runState%d" state
        let preamble = if isInit then "let rec" else "and"
        fprintfn writer "%s %s (st: State%d) =" preamble functionName state
        indent writer
        let assembleState state var =
            let vars = Map.find state stateVarMap |> fst |> List.map fst
            if List.isEmpty vars
            then fprintfn writer "()"
            else
                fprintfn writer "{"
                indent writer
                List.iter (fun v -> fprintfn writer "%s = %s;" v (if v = var then v else "st." + v)) vars
                unindent writer
                fprintfn writer "}"
        let generateForTransition t =
            match t with
            | {action = a; payload = p; label = l; toState = toState} ->
            if List.length p = 1
            then
                let var, ty = List.head p
                let ty = resolveTypeAlias ty
                match a with
                | Send ->
                    //fprintfn writer "send_string \"%s\"" l
                    let callbackName = sprintf "state%dOnsend%s" state l
                    fprintfn writer "let %s = callbacks.%s st" var callbackName
                    fprintfn writer "comms.send_%s %s" ty var
                | Receive ->
                    //fprintfn writer "let label = recv_string ()"
                    //fprintfn writer "assert (label = \"%s\")" l
                    let callbackName = sprintf "state%dOnreceive%s" state l
                    fprintfn writer "let %s = comms.recv_%s ()" var ty
                    fprintfn writer "callbacks.%s st %s" callbackName (if isDummy var then "" else var)
                | _ -> failwith "TODO"
                fprintf writer "let st : State%d = " toState
                assembleState toState var
                fprintfn writer "runState%d st" toState
            else failwith "Currently only support single payload"
        if List.contains state finalStates
        then
            writeln writer "()"
        else
            let stateTransition = Map.find state transitions
            if List.length stateTransition = 1
            then (* Singleton *)
                generateForTransition (List.head stateTransition)
            else (* Branch and Select *)
                match List.head stateTransition with
                (* From Scribble, we know that the action of all outgoing transitions must be the same *)
                | {action = Send} ->
                    let generateCase transition =
                        let label = transition.label
                        fprintfn writer "| State%dChoice.%s ->" state label
                        indent writer
                        fprintfn writer "comms.send_string \"%s\"" label
                        fprintf writer "let st : State%d_%s = " state label
                        assembleState state ""
                        generateForTransition transition
                        unindent writer
                    fprintfn writer "let label = callbacks.state%d st" state
                    fprintfn writer "match label with"
                    indent writer
                    List.iter generateCase stateTransition
                    unindent writer
                | {action = Receive} ->
                    let generateCase transition =
                        let label = transition.label
                        fprintfn writer "| \"%s\" ->" label
                        indent writer
                        generateForTransition transition
                        unindent writer
                    fprintfn writer "let label = comms.recv_string ()"
                    fprintfn writer "match label with"
                    indent writer
                    List.iter generateCase stateTransition
                    unindent writer
                | _ -> writeln writer "TODO"
        unindent writer
        false


    let generateRuntimeCode writer (cfsm : CFSM) stateVarMap =
        let initState, _, _ = cfsm
        let states = allStates cfsm
        indent writer
        printfn "%A" cfsm
        List.fold (generateRunState writer cfsm stateVarMap) true states |> ignore
        fprintfn writer "runState%d ()" initState
        unindent writer

    let generateCode (cfsm : CFSM) protocol localRole legacyApi =
        use fileWriter = new StreamWriter(!fileName)
        use writer = new IndentedTextWriter(fileWriter)
        let stateVarMap = CFSMAnalysis.constructVariableMap cfsm
        let stateVarMap = cleanUpVarMap stateVarMap
        generatePreamble writer !moduleName protocol localRole
        let content = generateCodeContent cfsm stateVarMap legacyApi localRole
        List.iter (writeContents writer) content
        if legacyApi
        then
            let init, _, _ = cfsm
            fprintfn writer "let init = %s" (mkStateName init)
        else
            (* TODO *)
            fprintfn writer "let run (callbacks : Callbacks%s) (comms : Communications) =" localRole
            generateRuntimeCode writer cfsm stateVarMap
        writer.Flush()
        ()
