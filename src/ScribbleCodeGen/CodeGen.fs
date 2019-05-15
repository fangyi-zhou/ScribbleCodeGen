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

    let mkStateName state =
        sprintf "State%d" state

    let allRoles ((_, transitions) : CFSM) =
        let accumRoles roles _ transitions =
            let newRoles = List.map (fun (t: Transition) -> t.partner) transitions
            Set.union (Set.ofList newRoles) roles
        Map.fold accumRoles Set.empty transitions

    let allStates : CFSM -> State list = snd >> Map.toList >> List.map (fst >> int)

    let isDummy (x : string) = x.StartsWith("_")

    let moduleName = ref "ScribbleGenerated"
    let fileName = ref "ScribbleGenerated.fs"

    let indent (writer: IndentedTextWriter) =
        writer.Indent <- writer.Indent + 1

    let unindent (writer: IndentedTextWriter) =
        writer.Indent <- writer.Indent - 1

    let writeln (writer: IndentedTextWriter) (str: string) =
        writer.Write(str)
        writer.WriteLine()

    let writeObject (writer: IndentedTextWriter) isFirst (name, (obj: Object))  =
        if isFirst then
            writeln writer (sprintf "type %s() = class" name)
            indent writer
        else
            writeln writer (sprintf "and %s() = class" name)
        List.iter (writeln writer) obj.members
        List.iter (writeln writer) obj.methods
        writeln writer "end"

    let writeContents (writer: IndentedTextWriter) (content: Content) =
        let content = Map.toSeq content
        if Seq.isEmpty content then ()
        else
            let first = Seq.head content
            let tail = Seq.tail content
            writeObject writer true (*isFirst*) first
            Seq.iter (writeObject writer false (*isFirst*)) tail
            unindent writer

    let writeRole (writer: IndentedTextWriter) (role: Role) =
        writeln writer (sprintf "type %s = %s" role role)

    let convertAction action =
        match action with
        | Send -> "send"
        | Receive -> "receive"
        | Accept -> "accept"
        | Request -> "request"

    let convertSinglePayload (var, ty) =
        let ty =
            match Map.tryFind ty defaultTypeAliasMap with
            | Some alias -> alias
            | None -> ty
        sprintf "%s : %s" var ty

    let convertPayload action payloads =
        match action with
        | Receive -> convertSinglePayload (List.head payloads)
        | Send -> List.filter (fst >> isDummy >> not) payloads |> List.map convertSinglePayload |> Seq.ofList |> String.concat ", "
        | _ -> failwith "TODO"

    let addSingleTransition (object: Object) (transition: Transition) =
        let toState = transition.toState
        let action = transition.action
        let partner = transition.partner
        let label = transition.label
        let payload = transition.payload
        let methodName = sprintf "%s%s" (convertAction action) label
        let methodArgs = convertPayload action ((partner, partner) :: payload)
        let method = sprintf "member __.%s(%s) : %s = failwith \"TODO\"" methodName methodArgs (mkStateName toState)
        { object with methods = method :: object.methods}

    let addTransition (content: Content) state transition =
        let stateName = mkStateName state
        let stateObj = Map.find stateName content
        let stateObj =
            if List.isEmpty transition
            then
                (* If a state has no transitions, then it must be a terminal state, we add `finish` here. *)
                let endMethod = "member __.finish() : End = End()"
                { stateObj with methods = endMethod :: stateObj.methods }
            else List.fold addSingleTransition stateObj transition
        Map.add stateName stateObj content

    let generateCode (cfsm : CFSM) protocol localRole =
        use fileWriter = new StreamWriter(!fileName)
        use writer = new IndentedTextWriter(fileWriter)
        writeln writer (sprintf "module %s%s%s" !moduleName  protocol localRole)
        writeln writer ("(* This file is GENERATED, do not modify manually *)")
        let init, transitions = cfsm
        let states = allStates cfsm
        let roles = allRoles cfsm
        Set.iter (writeRole writer) roles
        let content : Content = List.map (fun state -> mkStateName state, newObject) states |> Map.ofList
        let content = Map.fold addTransition content transitions
        let content = Map.add "End" newObject content (* The `End` object marks the end of communication *)
        writeContents writer content
        writeln writer (sprintf "let init = %s" (mkStateName init))
        writer.Flush()
        ()
