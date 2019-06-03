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

    let addTransitionCallback stateVarMap callbacks state transition =
        let callbacks =
            if stateHasInternalChoice transition then
                let field = sprintf "state%d" state
                let fieldType = sprintf "State%d -> State%dChoice" state state
                (field, fieldType, None) :: callbacks
            else callbacks
        List.fold (addSingleTransitionCallback stateVarMap) callbacks transition

    let addStateRecords stateVarMap content =
        let mkRecord (vars, assertions) =
            let rec aux (vars, assertions) refinedPayload =
                match vars with
                | [] ->
                    if List.isEmpty assertions
                    then refinedPayload
                    else failwith "Invalid CFSM"
                | (var, ty) :: rest ->
                    let knownVars = List.map (fun (v, _, _) -> v) refinedPayload
                    let boundVars = Set.add var (Set.ofList knownVars)
                    let isRefinementClosed term = Set.isSubset (FreeVar.free_var_term term) boundVars
                    let closed, notClosed = List.partition isRefinementClosed assertions
                    let newPayloadItem = var, ty, CFSMAnalysis.makeRefinementAttribute var ty closed
                    aux (rest, notClosed) (newPayloadItem :: refinedPayload)
            Record (aux (vars, assertions) [])
        Map.fold (fun content state stateVar -> Map.add (mkStateName state) (mkRecord stateVar) content) content stateVarMap

    let addInternalChoices content state transition =
        if stateHasInternalChoice transition
        then
            let counter = ref 0
            let makeChoiceEnumItem (transition : Transition) =
                let enumValue = !counter
                counter := !counter + 1
                sprintf "%s = %d" transition.label enumValue, [], None
            let choices = List.map makeChoiceEnumItem transition
            let union = Union choices
            Map.add (sprintf "State%dChoice" state) union content
        else
            content

    let generateCodeContentEventStyleApi cfsm localRole =
        let _, transitions = cfsm
        let states = allStates cfsm
        let roles = allRoles cfsm
        let stateVarMap = CFSMAnalysis.constructVariableMap cfsm
        let stateVarMap = cleanUpVarMap stateVarMap
        let content = Map.empty
        assert (List.length states = Map.count stateVarMap)
        let content = addStateRecords stateVarMap content
        let content = Set.fold addRole content roles
        let content = Map.fold addInternalChoices content transitions
        let callbacks = Map.fold (addTransitionCallback stateVarMap) [] transitions |> List.rev
        let callbacks = Map.ofList ["Callbacks" + localRole, (Record callbacks)]
        [content; callbacks]
