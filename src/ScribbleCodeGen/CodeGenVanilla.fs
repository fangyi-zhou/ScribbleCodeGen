namespace ScribbleCodeGen

open CodeGenCommon

module CodeGenVanilla =

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

    let generateCodeContentVanillaApi cfsm =
        let _, transitions = cfsm
        let states = allStates cfsm
        let roles = allRoles cfsm
        let content : Content = List.map (fun state -> mkStateName state, newObject) states |> Map.ofList
        let content = Set.fold addRole content roles
        let content = Map.fold addTransition content transitions
        let content = Map.add "End" newObject content (* The `End` object marks the end of communication *)
        content
