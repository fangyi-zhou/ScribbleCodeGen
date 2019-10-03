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
        let eff = if !codeGenMode = FStar then "ML " else ""
        sprintf "%s -> %s%s" argType eff retType

    let getCallbackRefinement state varMap transition =
        let action = transition.action
        let payload = transition.payload |> List.filter (fst >> isDummy >> not)
        let binder (v: Variable) =
            match !codeGenMode with
            | FStar -> App (Var (sprintf "Mk%s?.%s" state v), (Var "st"))
            | _ -> FieldGet (Var "st", v)
        let payload, _ = CFSMAnalysis.attachRefinements transition.assertion varMap payload (Some binder) !codeGenMode
        let argType =
            match action with
            | Send -> sprintf "(st: %s)" state
            | Receive -> curriedPayloadRefined (("st", state, None) :: payload)
            | _ -> failwith "TODO"
        let retType =
            match action with
            | Send -> productOfRefinedPayload payload
            | Receive -> "unit"
            | _ -> failwith "TODO"
        match !codeGenMode with
        | FStar ->
            sprintf "%s -> ML (%s)" argType retType
        | _ ->
            sprintf "%s -> %s" argType retType

    let addSingleTransitionCallback stateVarMap callbacks transition =
        let action = convertAction transition.action
        let fromState = transition.fromState
        let label = transition.label
        let field = sprintf "state%dOn%s%s" fromState action label
        let state = mkStateName fromState
        let fieldType = getCallbackType state transition
        let refinement = getCallbackRefinement state (Map.find fromState stateVarMap) transition
        match !codeGenMode with
        | FStar ->
            (field, refinement, None) :: callbacks
        | _ ->
            (field, fieldType, Some refinement) :: callbacks

    let getChoiceRefinement state vars transition =
        let mkDisjunction cases =
            match cases with
            | [] -> mk_bool false
            | hd :: tl -> List.fold (mk_binop_app Or) hd tl
        let mkConjunction cases =
            match cases with
            | [] -> mk_bool true
            | hd :: tl -> List.fold (mk_binop_app And) hd tl
        let mkCase transition =
            let preconditions = List.filter (fun e -> Set.isSubset (FreeVar.free_var_term e) vars) transition.assertion
            let predicates = (mk_binop_app EqualInt (Var "choice") (Var (sprintf "Choice%d%s" state transition.label))) :: preconditions
            mkConjunction predicates
        let cases = List.map mkCase transition
        let refinementTerm = mkDisjunction cases
        let freeVars = FreeVar.free_var_term refinementTerm
        let varsToBind = Set.intersect vars freeVars
        let binder (v: Variable) =
            match !codeGenMode with
            | FStar -> App (Var (sprintf "Mkstate%d?.%s" state v), (Var "st"))
            | _ -> FieldGet (Var "st", v)
        let refinementTerm = Set.fold (fun term var -> Substitution.substitute_term term var (binder var)) refinementTerm varsToBind
        match !codeGenMode with
        | FStar ->
            sprintf "(st: state%d) -> ML (choice:state%dChoice{%s})" state state (CFSMAnalysis.termToString refinementTerm)
        | _ ->
            sprintf "(st: State%d) -> {choice:int|%s}" state (CFSMAnalysis.termToString refinementTerm)

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
        match !codeGenMode with
        | FStar ->
            (field, refinement, None) :: callbacks
        | _ ->
            (field, fieldType, Some refinement) :: callbacks

    let addTransitionCallback stateVarMap callbacks state transition =
        if stateHasInternalChoice transition then
            let field = sprintf "state%d" state
            let s = if !codeGenMode = FStar then 's' else 'S'
            let eff = if !codeGenMode = FStar then "ML " else ""
            let fieldType = sprintf "%ctate%d -> %s%ctate%dChoice" s state eff s state
            let currentStateVars = Map.find state stateVarMap |> fst |> List.map fst |> Set.ofList
            let refinement = getChoiceRefinement state currentStateVars transition
            let callbacks =
                match !codeGenMode with
                | FStar -> (field, refinement, None) :: callbacks (* nasty HACK *)
                | _ -> (field, fieldType, Some refinement) :: callbacks
            List.fold (addSingleInternalChoiceSendCallback stateVarMap) callbacks transition
        else
            List.fold (addSingleTransitionCallback stateVarMap) callbacks transition

    let mkStateRecord (vars, assertions) =
        let rec aux (vars, assertions) refinedPayload =
            match vars with
            | [] ->
                if not (List.isEmpty assertions)
                then eprintfn "Dropped assertions %A" assertions
                List.rev refinedPayload
            | (var, ty) :: rest ->
                let knownVars = List.map (fun (v, _, _) -> v) refinedPayload
                let boundVars = Set.add var (Set.ofList knownVars)
                let isRefinementClosed term = Set.isSubset (FreeVar.free_var_term term) boundVars
                let closed, notClosed = List.partition isRefinementClosed assertions
                let newPayloadItem = var, ty, CFSMAnalysis.makeRefinementAttribute var ty closed !codeGenMode
                aux (rest, notClosed) (newPayloadItem :: refinedPayload)
        Record (aux (vars, assertions) [])

    let addStateRecords stateVarMap content =
        Map.fold (fun content state stateVar -> Map.add (mkStateName state) (mkStateRecord stateVar) content) content stateVarMap

    let addSendStatePredicate stateVarMap state content transition =
        let vars, assertions = Map.find state stateVarMap
        let currentStateVars = Map.find state stateVarMap |> fst |> List.map fst |> Set.ofList
        let preconditions = List.filter (fun e -> Set.isSubset (FreeVar.free_var_term e) currentStateVars) transition.assertion
        let record = mkStateRecord (vars, assertions @ preconditions)
        let s = if !codeGenMode = FStar then 's' else 'S'
        let recordName = sprintf "%ctate%d_%s" s state transition.label
        Map.add recordName record content

    let addInternalChoices stateVarMap content state transition =
        if stateHasInternalChoice transition
        then
            let choices =
                if !codeGenMode <> FStar
                then
                    let counter = ref 0
                    let makeChoiceEnumItem (transition : Transition) =
                        let enumValue = !counter
                        counter := !counter + 1
                        sprintf "%s = %d" transition.label enumValue, [], None
                    List.map makeChoiceEnumItem transition
                else
                    List.map (fun t -> sprintf "Choice%d%s" state t.label, [], None) transition
            let union = Union choices
            let content = Map.add (sprintf "State%dChoice" state) union content
            List.fold (addSendStatePredicate stateVarMap state) content transition
        else
            content

    let generateCodeContentEventStyleApi cfsm stateVarMap localRole =
        let _, _, transitions = cfsm
        let states = allStates cfsm
        let roles = allRoles cfsm
        assert (List.length states = Map.count stateVarMap)
        let stateRecords = addStateRecords stateVarMap Map.empty
        let roles = addRole Map.empty roles
        let choices = Map.fold (addInternalChoices stateVarMap) Map.empty transitions
        let callbacks = Map.fold (addTransitionCallback stateVarMap) [] transitions |> List.rev
        let callbacks = Map.ofList ["Callbacks" + localRole, (Record callbacks)]
        [roles; choices; stateRecords; callbacks]
