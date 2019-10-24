namespace ScribbleCodeGen

open FluidTypes.Refinements
open FluidTypes.Annotations.AnnotationParser

module CFSMConversion =
    let newSyntax = ref false

    let newTransitionMap = Map.empty

    let rec cutAssertion assertion =
        match assertion with
        | App (App (Const (Binop And), t1), t2) ->
            cutAssertion t1 @ cutAssertion t2
        | Const (BoolLiteral true) -> []
        | assertion -> [assertion]

    let addTransition (transitions: TransitionMap) (transition: Transition) : TransitionMap =
        let from = transition.fromState
        match Map.tryFind from transitions with
        | Some oldTransitions -> Map.add from (transition :: oldTransitions) transitions
        | None -> Map.add from [transition] transitions

    let parseAssertionAndChunk assertion =
        if assertion = ""
        then []
        else
            try
                let term = parse_term assertion
                cutAssertion term
            with _ ->
                eprintfn "Cannot parse %s, dropping" assertion
                []

    let parseTransition fromState toState label : Transition =
        let partner, action, label, payload, assertionString, recVarExpr =
            if not !newSyntax
                then Parsing.parseOldDotLabel label
                else Parsing.parseNewDotLabel label
        let recVarExpr = List.map parse_term recVarExpr
        {
            fromState = fromState
            toState = toState
            partner = partner
            action = action
            label = label
            payload = payload
            assertion = parseAssertionAndChunk assertionString
            recVarExpr = recVarExpr
        }

    let convertEdge (transitions : TransitionMap) (fromState, toState) attributes =
        let processAttribute transitions attribute =
            let fromState = int fromState
            let toState = int toState
            let label = Map.find "label" attribute
            let transition = parseTransition fromState toState label
            addTransition transitions transition
        List.fold processAttribute transitions attributes

    let convertNode (recVarMap: RecVarMap) state attributes =
        if not !newSyntax
        then
            recVarMap
        else
            let state = int state
            let label = Map.find "label" attributes
            let recvars, assertion = Parsing.parseRecVarEntry label
            let assertion = parseAssertionAndChunk assertion
            Map.add state (recvars, assertion) recVarMap

    let convert (graph: DotParser.GraphData) (recursiveRefinement: bool) : CFSM =
        newSyntax := recursiveRefinement
        let edges = graph.Edges
        let nodes = graph.Nodes
        let states = nodes |> Map.toList |> List.map (fst >> int)
        let init = List.min states
        let recVarMap = Map.fold convertNode Map.empty nodes
        let initMap = List.map (fun state -> state, []) states |> Map.ofList
        let transitionMap = Map.fold convertEdge initMap edges
        let finals = Map.filter (fun _ trans -> List.isEmpty trans) transitionMap |> Map.toList |> List.map fst
        init, finals, transitionMap, recVarMap
