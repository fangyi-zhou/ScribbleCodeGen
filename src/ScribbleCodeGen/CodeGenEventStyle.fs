namespace ScribbleCodeGen

open FluidTypes.Refinements
open CodeGenCommon

module CodeGenEventStyle =

    let getCallbackType transition =
        let state = mkStateName transition.fromState
        let action = transition.action
        let payload = transition.payload |> List.filter (fst >> isDummy >> not)
        let argType =
            match action with
            | Send -> state
            | Receive -> curriedPayload (("state", state) :: payload)
            | _ -> failwith "TODO"
        let retType =
            match action with
            | Send -> productOfPayload payload
            | Receive -> "unit"
            | _ -> failwith "TODO"
        sprintf "%s -> %s" argType retType

    let getCallbackRefinement varMap transition =
        let state = mkStateName transition.fromState
        let action = transition.action
        let payload = transition.payload |> List.filter (fst >> isDummy >> not)
        let binder (v: Variable) = FieldGet (Var "state", v)
        let payload, _ = CFSMAnalysis.attachRefinements transition.assertion varMap payload (Some binder)
        let argType =
            match action with
            | Send -> sprintf "(state : %s)" state
            | Receive -> curriedPayloadRefined (("state", state, None) :: payload)
            | _ -> failwith "TODO"
        let retType =
            match action with
            | Send -> productOfRefinedPayload payload
            | Receive -> "unit"
            | _ -> failwith "TODO"
        sprintf "%s -> %s" argType retType

    let addSingleTransitionCallback stateVarMap callbacks transition =
        let action = convertAction transition.action
        let fromState = transition.fromState
        let label = transition.label
        let field = sprintf "state%dOn%s%s" fromState action label
        let fieldType = getCallbackType transition
        let refinement = getCallbackRefinement (Map.find fromState stateVarMap) transition
        (field, fieldType, Some refinement) :: callbacks (* TODO: Add refinement type *)

    let addTransitionCallback stateVarMap callbacks _ transition =
        List.fold (addSingleTransitionCallback stateVarMap) callbacks transition

    let addStateRecords stateVarMap content =
        Map.fold (fun content state varDef -> Map.add (mkStateName state) (Record varDef) content) content stateVarMap

    let generateCodeContentEventStyleApi cfsm =
        let _, transitions = cfsm
        let states = allStates cfsm
        let roles = allRoles cfsm
        let stateVarMap = CFSMAnalysis.constructVariableMap cfsm
        let stateVarMap = cleanUpVarMap stateVarMap
        let content = Map.empty
        assert (List.length states = Map.count stateVarMap)
        let content = addStateRecords stateVarMap content
        let content = Set.fold addRole content roles
        let callbacks = Map.fold (addTransitionCallback stateVarMap) [] transitions |> List.rev
        let content = Map.add "Callbacks" (Record callbacks) content
        content
