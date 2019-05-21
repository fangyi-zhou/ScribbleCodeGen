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

    let attachRefinements refinements varMap payloads =
        let addVariableWithRefinements (refinements, knownVars) (var, ty) =
            let boundVars = Set.add var (Set.ofList (List.map (fun (v, _, _) -> v) knownVars))
            let isRefinementClosed term = Set.isSubset (FreeVar.free_var_term term) boundVars
            let closed, notClosed = List.partition isRefinementClosed refinements
            let newPayloadItem = var, ty, makeRefinementAttribute var ty closed
            newPayloadItem, (notClosed, (newPayloadItem :: knownVars))
        List.mapFold (addVariableWithRefinements) (refinements, varMap) payloads

    let constructVariableMap (cfsm : CFSM) : StateVariableMap =
        let init, allTransitions = cfsm
        let rec aux (varMap : StateVariableMap) state vars =
            if Map.containsKey state varMap
            then varMap
            else
                let varMap = Map.add state vars varMap
                let transitions = Map.find state allTransitions
                let addVariablesFromTransition transition =
                    let fromState = transition.fromState
                    let fromStateVars = Map.find fromState varMap
                    let payloads = transition.payload
                    let refinements = transition.assertion
                    attachRefinements refinements fromStateVars payloads |> snd |> snd
                List.fold (fun varMap transition -> aux varMap transition.toState (addVariablesFromTransition transition)) varMap transitions
        aux Map.empty init []
