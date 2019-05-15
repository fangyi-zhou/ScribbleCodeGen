namespace ScribbleCodeGen

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
        { object with methods = method :: object.methods }

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

    let addRole content role =
        let roleUnion = Union [role]
        Map.add role roleUnion content

    let generateCodeContentVanillaApi cfsm =
        let _, transitions = cfsm
        let states = allStates cfsm
        let roles = allRoles cfsm
        let content : Content = List.map (fun state -> mkStateName state, newObject) states |> Map.ofList
        let content = Set.fold addRole content roles
        let content = Map.fold addTransition content transitions
        let content = Map.add "End" newObject content (* The `End` object marks the end of communication *)
        content

    let generateCodeContentEventStyleApi cfsm =
        failwith "TODO"

    let generateCodeContent (cfsm : CFSM) eventStyleApi =
        if eventStyleApi
            then generateCodeContentEventStyleApi cfsm
            else generateCodeContentVanillaApi cfsm
