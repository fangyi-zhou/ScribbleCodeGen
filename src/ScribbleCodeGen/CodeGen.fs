namespace ScribbleCodeGen
open System.IO
open System.CodeDom.Compiler

module CodeGen =

    type Method = string
    type Member = string
    type UnionCase = string

    type Object = {
        methods : Method list
        members : Member list
    }

    type TypeDef =
        | Object of Object
        | Union of UnionCase list

    type Content = Map<string, TypeDef>

    let newObject = Object {
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

    let writeObject (writer: IndentedTextWriter) isFirst name obj =
        if isFirst then
            writeln writer (sprintf "type %s() = class" name)
        else
            writeln writer (sprintf "and %s() = class" name)
        indent writer
        List.iter (writeln writer) obj.members
        List.iter (writeln writer) obj.methods
        writeln writer "end"
        unindent writer

    let writeUnion (writer: IndentedTextWriter) isFirst name union =
        if isFirst then
            writeln writer (sprintf "type %s =" name)
        else
            writeln writer (sprintf "and %s =" name)
        indent writer
        List.iter (fun unioncase -> writeln writer (sprintf "| %s" unioncase)) union
        unindent writer

    let writeTypeDef (writer: IndentedTextWriter) isFirst (name, typeDef) =
        match typeDef with
        | Union u -> writeUnion writer isFirst name u
        | Object o -> writeObject writer isFirst name o

    let writeContents (writer: IndentedTextWriter) (content: Content) =
        let content = Map.toList content
        match content with
            | [] -> ()
            | first :: rest ->
                writeTypeDef writer true first
                List.iter (writeTypeDef writer false) rest

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

    let stateHasExternalChoice transitions =
        let receiveCount = List.filter (fun t -> t.action = Receive) >> List.length
        receiveCount transitions > 1

    let generateChoices (content: Content) state transition =
        let labels = List.distinct (List.map (fun t -> t.label, t.toState) transition)
        let mkLabelUnionCaseName (label, toState) = sprintf "Choice%d%s of %s" state label (mkStateName toState)
        let labelUnionCases = List.map mkLabelUnionCaseName labels
        let unionName = sprintf "Choice%d" state
        Map.add unionName (Union labelUnionCases) content

    let addTransition (content: Content) state transition =
        let stateName = mkStateName state
        let hasExternalChoice = stateHasExternalChoice transition
        let stateObj = Map.find stateName content
        let stateObj =
            match stateObj with
            | Object o -> o
            | _ -> failwithf "Expecting an object for state %d" state
        let stateObj =
            match transition with
            | _ when List.isEmpty transition ->
                (* If a state has no transitions, then it must be a terminal state, we add `finish` here. *)
                let endMethod = "member __.finish() : End = End()"
                { stateObj with methods = endMethod :: stateObj.methods }
            | _ when hasExternalChoice ->
                let branchMethod = sprintf "member __.branch() : Choice%d = failwith \"TODO\"" state
                { stateObj with methods = branchMethod :: stateObj.methods }
            | _ ->
                List.fold addSingleTransition stateObj transition
        let content = Map.add stateName (Object stateObj) content
        if not hasExternalChoice
            then content
            else generateChoices content state transition

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
