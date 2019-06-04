namespace ScribbleCodeGen

open FluidTypes.Refinements
open CodeGenCommon

module CodeGenEventStyle =

    let getCallbackType state transition =
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

    let getCallbackRefinement state varMap transition =
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
        let state = mkStateName fromState
        let fieldType = getCallbackType state transition
        let refinement = getCallbackRefinement state (Map.find fromState stateVarMap) transition
        (field, fieldType, Some refinement) :: callbacks

    let getChoiceRefinement state vars transition =
        let mkDisjunction cases = List.fold (fun e1 e2 -> mk_binop_app Or e1 e2) (mk_bool false) cases
        let mkConjunction cases = List.fold (fun e1 e2 -> mk_binop_app And e1 e2) (mk_bool true) cases
        let mkCase idx transition =
            let preconditions = List.filter (fun e -> Set.isSubset (FreeVar.free_var_term e) vars) transition.assertion
            let predicates = (mk_binop_app EqualInt (Var "choice") (mk_int idx)) :: preconditions
            mkConjunction predicates
        let cases = List.mapi mkCase transition
        let refinementTerm = mkDisjunction cases
        let freeVars = FreeVar.free_var_term refinementTerm
        let varsToBind = Set.intersect vars freeVars
        let binder v = FieldGet (Var "state", v)
        let refinementTerm = Set.fold (fun term var -> Substitution.substitute_term term var (binder var)) refinementTerm varsToBind
        sprintf "(state: State%d) -> {choice:int|%s}" state (CFSMAnalysis.termToString refinementTerm)

    let addSingleInternalChoiceSendCallback stateVarMap callbacks transition =
        (* TODO: Refactor *)
        let action = convertAction transition.action
        let fromState = transition.fromState
        let label = transition.label
        let field = sprintf "state%dOn%s%s" fromState action label
        let state = sprintf "%s_%s" (mkStateName fromState) transition.label
        let fieldType = getCallbackType state transition
        (* TODO: Remove those refinements in predicate *)
        let refinement = getCallbackRefinement state (Map.find fromState stateVarMap) transition
        (field, fieldType, Some refinement) :: callbacks

    let addTransitionCallback stateVarMap callbacks state transition =
        if stateHasInternalChoice transition then
            let field = sprintf "state%d" state
            let fieldType = sprintf "State%d -> State%dChoice" state state
            let currentStateVars = Map.find state stateVarMap |> fst |> List.map fst |> Set.ofList
            let refinement = getChoiceRefinement state currentStateVars transition
            let callbacks = (field, fieldType, Some refinement) :: callbacks
            List.fold (addSingleInternalChoiceSendCallback stateVarMap) callbacks transition
        else
            List.fold (addSingleTransitionCallback stateVarMap) callbacks transition

    let mkStateRecord (vars, assertions) =
        let rec aux (vars, assertions) refinedPayload =
            match vars with
            | [] ->
                if List.isEmpty assertions
                then List.rev refinedPayload
                else failwith "Invalid CFSM"
            | (var, ty) :: rest ->
                let knownVars = List.map (fun (v, _, _) -> v) refinedPayload
                let boundVars = Set.add var (Set.ofList knownVars)
                let isRefinementClosed term = Set.isSubset (FreeVar.free_var_term term) boundVars
                let closed, notClosed = List.partition isRefinementClosed assertions
                let newPayloadItem = var, ty, CFSMAnalysis.makeRefinementAttribute var ty closed
                aux (rest, notClosed) (newPayloadItem :: refinedPayload)
        Record (aux (vars, assertions) [])

    let addStateRecords stateVarMap content =
        Map.fold (fun content state stateVar -> Map.add (mkStateName state) (mkStateRecord stateVar) content) content stateVarMap

    let addSendStatePredicate stateVarMap state content transition =
        let vars, assertions = Map.find state stateVarMap
        let currentStateVars = Map.find state stateVarMap |> fst |> List.map fst |> Set.ofList
        let preconditions = List.filter (fun e -> Set.isSubset (FreeVar.free_var_term e) currentStateVars) transition.assertion
        let record = mkStateRecord (vars, assertions @ preconditions)
        let recordName = sprintf "State%d_%s" state transition.label
        Map.add recordName record content

    let addInternalChoices stateVarMap content state transition =
        if stateHasInternalChoice transition
        then
            let counter = ref 0
            let makeChoiceEnumItem (transition : Transition) =
                let enumValue = !counter
                counter := !counter + 1
                sprintf "%s = %d" transition.label enumValue, [], None
            let choices = List.map makeChoiceEnumItem transition
            let union = Union choices
            let content = Map.add (sprintf "State%dChoice" state) union content
            List.fold (addSendStatePredicate stateVarMap state) content transition
        else
            content

    let generateCodeContentEventStyleApi cfsm localRole =
        let _, _, transitions = cfsm
        let states = allStates cfsm
        let roles = allRoles cfsm
        let stateVarMap = CFSMAnalysis.constructVariableMap cfsm
        let stateVarMap = cleanUpVarMap stateVarMap
        let content = Map.empty
        assert (List.length states = Map.count stateVarMap)
        let content = addStateRecords stateVarMap content
        let content = Set.fold addRole content roles
        let content = Map.fold (addInternalChoices stateVarMap) content transitions
        let callbacks = Map.fold (addTransitionCallback stateVarMap) [] transitions |> List.rev
        let callbacks = Map.ofList ["Callbacks" + localRole, (Record callbacks)]
        [content; callbacks]
