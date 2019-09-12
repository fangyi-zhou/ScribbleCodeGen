namespace ScribbleCodeGen

open System.IO
open System.CodeDom.Compiler

open FluidTypes.Refinements
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

    let ensureStartsWithLowerCase (string : string) =
        match string.[0] with
        | ch when System.Char.IsLower (ch) -> string
        | ch -> sprintf "%c%s" (System.Char.ToLower (ch)) string.[1..]

    let writeTypeDefPreamble (writer: IndentedTextWriter) isFirst (name: string) content =
        let noeq = if name.StartsWith("Callbacks") then "noeq " else "" (* Yet another nasty HACK *)
        let preamble =
            if isFirst then noeq + "type" else "and"
        let name = if !codeGenMode = FStar then ensureStartsWithLowerCase name else name
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
        match !codeGenMode with
        | FStar ->
            let refinedType =
                match refinement with
                | Some refinement ->
                    sprintf "(%s : %s{%s})" field fieldType refinement
                | None -> fieldType
            fprintfn writer "%s : %s;" field refinedType
        | _ ->
            let refinementAttribute =
                match refinement with
                | Some refinement ->
                    sprintf "[<Refined(\"%s\")>] " refinement
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
            (* FIXME: Hack *)
            if !codeGenMode = FStar && name.StartsWith("state")
            then fprintfn writer ("_dum%s : unit;") name
            List.iter (writeRecordItem writer) record
            unindent writer
            writeln writer "}"

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
        let moduleName =
            match !codeGenMode with
            | FStar -> (Seq.takeWhile ( (<>) '.' ) !fileName) |> System.String.Concat
            | _ -> sprintf "%s%s%s" moduleName protocol localRole
        fprintfn writer "module %s" moduleName
        writeln writer ("(* This file is GENERATED, do not modify manually *)")
        if !codeGenMode <> FStar then writeln writer ("open FluidTypes.Annotations") else writeln writer ("open FStar.All"); writeln writer ("open FStar.Error")
        writeln writer ""
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
        let stateTy = if !codeGenMode = FStar then "state" else "State"
        let in_ = if !codeGenMode = FStar then " in" else ""
        let in__ () = if !codeGenMode = FStar then fprintfn writer "in"
        let semi_ = if !codeGenMode = FStar then ";" else ""
        fprintfn writer "%s %s (st: %s%d) %s=" preamble functionName stateTy state (if !codeGenMode = FStar then ": ML unit " else "")
        indent writer
        let fieldGet field stateName =
            if !codeGenMode = FStar
            then sprintf "(Mk%s?.%s st)" stateName field
            else "st." + field
        let assembleState state var stateTy prevStateTy =
            let vars = Map.find state stateVarMap |> fst |> List.map fst
            if List.isEmpty vars
            then fprintfn writer "()"
            else
                fprintfn writer "{"
                indent writer
                if !codeGenMode = FStar
                then fprintfn writer "_dum%s = ();" stateTy
                List.iter (fun v -> fprintfn writer "%s = %s;" v (if v = var then v else fieldGet v prevStateTy)) vars
                unindent writer
                fprintfn writer "}"
        let generateForTransition t prevStateName =
            match t with
            | {action = a; payload = p; label = l; toState = toState; partner = r} ->
            if List.length p = 1
            then
                let var, ty = List.head p
                let ty = resolveTypeAlias ty
                match a with
                | Send ->
                    //fprintfn writer "send_string \"%s\"" l
                    let callbackName = sprintf "state%dOnsend%s" state l
                    fprintfn writer "let %s = callbacks.%s st%s" var callbackName in_
                    fprintfn writer "comms.send_%s %s %s%s" ty r var semi_
                | Receive ->
                    //fprintfn writer "let label = recv_string ()"
                    //fprintfn writer "assert (label = \"%s\")" l
                    let callbackName = sprintf "state%dOnreceive%s" state l
                    fprintfn writer "let %s = comms.recv_%s %s ()%s" var ty r in_
                    if !codeGenMode = FStar
                    then
                        let binder (v: Variable) = App (Var (sprintf "Mkstate%d?.%s" state v), (Var "st"))
                        let varMap = Map.find state stateVarMap
                        let payload, _ = CFSMAnalysis.attachRefinements t.assertion varMap p (Some binder) !codeGenMode
                        match payload with
                        | [_, _, Some r] -> fprintfn writer "assume (%s);" r
                        | [_, _, None] -> ()
                        | _ -> failwith "Unreachable"
                    fprintfn writer "callbacks.%s st %s%s" callbackName (if isDummy var then "" else var) semi_
                | _ -> failwith "TODO"
                let stateTyName = sprintf "%s%d" stateTy toState
                fprintf writer "let st : %s = " stateTyName
                let prevStateName = Option.defaultValue (sprintf "state%d" state) prevStateName
                assembleState toState var stateTyName prevStateName
                in__ ()
                fprintfn writer "runState%d st" toState
            else failwith "Currently only support single payload"
        if List.contains state finalStates
        then
            writeln writer "()"
        else
            let stateTransition = Map.find state transitions
            if List.length stateTransition = 1
            then (* Singleton *)
                generateForTransition (List.head stateTransition) None
            else (* Branch and Select *)
                match List.head stateTransition with
                (* From Scribble, we know that the action of all outgoing transitions must be the same *)
                | {action = Send} ->
                    let generateCase transition =
                        let label = transition.label
                        let role = transition.partner
                        if !codeGenMode = FStar
                        then
                            fprintfn writer "| Choice%d%s ->" state label
                        else
                            fprintfn writer "| State%dChoice.%s ->" state label
                        indent writer
                        fprintfn writer "comms.send_string %s \"%s\"%s" role label semi_
                        let stateTyName = sprintf "%s%d_%s" stateTy state label
                        fprintf writer "let st : %s = " stateTyName
                        assembleState state "" stateTyName (sprintf "state%d" state)
                        in__ ()
                        generateForTransition transition (Some stateTyName)
                        unindent writer
                    fprintfn writer "let label = callbacks.state%d st%s" state in_
                    fprintfn writer "match label with"
                    indent writer
                    List.iter generateCase stateTransition
                    unindent writer
                | {action = Receive; partner = role} ->
                    let generateCase transition =
                        let label = transition.label
                        fprintfn writer "| \"%s\" ->" label
                        indent writer
                        generateForTransition transition None
                        unindent writer
                    fprintfn writer "let label = comms.recv_string %s ()%s" role in_
                    fprintfn writer "match label with"
                    indent writer
                    List.iter generateCase stateTransition
                    fprintfn writer "| _ -> unexpected \"unexpected label\""
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
        if !codeGenMode = FStar
        then fprintfn writer "in"
        fprintfn writer "runState%d ()" initState
        unindent writer

    let writeCommunicationDef writer =
        let noeq = if !codeGenMode = FStar then "noeq " else ""
        let comm = if !codeGenMode = FStar then "communications" else "Communications"
        let role = if !codeGenMode = FStar then "role" else "Role"
        let eff = if !codeGenMode = FStar then "ML " else ""
        fprintfn writer "%stype %s = {" noeq comm
        fprintfn writer "    send_int : %s -> int -> %sunit;" role eff
        fprintfn writer "    send_string : %s -> string -> %sunit;" role eff
        fprintfn writer "    send_unit : %s -> unit -> %sunit;" role eff
        fprintfn writer "    recv_int : %s -> unit -> %sint;" role eff
        fprintfn writer "    recv_string : %s -> unit -> %sstring;" role eff
        fprintfn writer "    recv_unit : %s -> unit -> %sunit;" role eff
        fprintfn writer "}"

    let generateCode (cfsm : CFSM) protocol localRole =
        use fileWriter = new StreamWriter(!fileName)
        use writer = new IndentedTextWriter(fileWriter)
        let stateVarMap = CFSMAnalysis.constructVariableMap cfsm
        let stateVarMap = cleanUpVarMap stateVarMap
        generatePreamble writer !moduleName protocol localRole
        let content = generateCodeContent cfsm stateVarMap localRole
        List.iter (writeContents writer) content
        writeCommunicationDef writer
        match !codeGenMode with
        | LegacyApi ->
            let init, _, _ = cfsm
            fprintfn writer "let init = %s" (mkStateName init)
        | EventApi ->
            fprintfn writer "let run (callbacks : Callbacks%s) (comms : Communications) =" localRole
            generateRuntimeCode writer cfsm stateVarMap
        | FStar ->
            (*TODO*)
            fprintfn writer "let run (callbacks : callbacks%s) (comms : communications) : ML unit =" localRole
            generateRuntimeCode writer cfsm stateVarMap
        writer.Flush()
        ()
