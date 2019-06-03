namespace ScribbleCodeGen

open FluidTypes.Refinements

module CFSMAnalysis =

    (* I know this should not be here, but whatever *)
    let rec termToString term =
        match term with
        | Var v -> sprintf "(%s)" v
        | Const (IntLiteral i) -> sprintf "(%d)" i
        | Const (BoolLiteral b) -> if b then "(true)" else "(false)"
        | App (App (Const (Binop b), t1), t2) -> sprintf "(%s %s %s)" (termToString t1) (binopToString b) (termToString t2)
        | App (Const (Unop u), t1) -> sprintf "(%s %s)" (unopToString u) (termToString t1)
        | FieldGet (t, v) -> sprintf "(%s$%s)" (termToString t) v
        | _ -> failwith "TODO"
    and binopToString b =
        match b with
        | Plus -> "+"
        | Minus -> "-"
        | EqualInt | EqualBool -> "="
        | NotEqualInt | NotEqualBool -> "<>"
        | And -> "&&"
        | Or -> "||"
        | Greater -> ">"
        | GreaterEqual -> ">="
        | Less -> "<"
        | LessEqual -> "<="
    and unopToString u =
        match u with
        | Negate -> "-"
        | Not -> "not"

    let makeRefinementAttribute var ty terms =
        match terms with
        | [] -> None
        | terms ->
            let terms = List.map termToString terms |> Seq.ofList
            let refinement = String.concat " && " terms
            Some (sprintf "{%s:%s|%s}" var ty refinement)

    let bindVars varsToBind binder term =
        let freeVars = FreeVar.free_var_term term
        let varsToBind = Set.intersect (Set.ofList varsToBind) freeVars
        Set.fold (fun term var -> Substitution.substitute_term term var (binder var)) term varsToBind

    let attachRefinements refinements (vars, _) payloads binder =
        let addVariableWithRefinements (refinements, existingPayload) (var, ty) =
            let varsToBind = List.map fst vars
            let knownVars = List.map (fun (v, _, _) -> v) existingPayload
            let boundVars = Set.add var (Set.ofList knownVars)
            let isRefinementClosed term = Set.isSubset (FreeVar.free_var_term term) boundVars
            let closed, notClosed = List.partition isRefinementClosed refinements
            let closed =
                match binder with
                | Some binder -> List.map (bindVars varsToBind binder) closed
                | None -> closed
            let newPayloadItem = var, ty, makeRefinementAttribute var ty closed
            newPayloadItem, (notClosed, (newPayloadItem :: existingPayload))
        List.mapFold addVariableWithRefinements (refinements, List.map (fun (x, y) -> x, y, None) vars) payloads

    let constructVariableMap (cfsm : CFSM) : StateVariableMap =
        let init, allTransitions = cfsm
        let rec aux (varMap : StateVariableMap) state (vars, assertions) =
            if Map.containsKey state varMap
            then varMap
            else
                let varMap = Map.add state (vars, assertions) varMap
                let transitions = Map.find state allTransitions
                let updateWithTransition transition =
                    let newAssertions = transition.assertion
                    let newVars = transition.payload
                    vars @ newVars, assertions @ newAssertions
                List.fold (fun varMap transition -> aux varMap transition.toState (updateWithTransition transition)) varMap transitions
        aux Map.empty init ([], [])
