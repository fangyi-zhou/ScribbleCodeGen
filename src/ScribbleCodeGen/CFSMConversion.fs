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
        | assertion -> [assertion]

    let addTransition (transitions: TransitionMap) (transition: Transition) : TransitionMap =
        let from = transition.fromState
        match Map.tryFind from transitions with
        | Some oldTransitions -> Map.add from (transition :: oldTransitions) transitions
        | None -> Map.add from [transition] transitions

    let parseTransition fromState toState label : Transition =
        let partner, action, label, payload, assertionString = 
            if not !newSyntax
                then Parsing.parseOldDotLabel label
                else Parsing.parseNewDotLabel label
        let parsedAssertion = try (Some (parse_term assertionString)) with e -> None
        let chunkedAssertions = Option.map cutAssertion parsedAssertion |> Option.defaultValue []
        {
            fromState = fromState
            toState = toState
            partner = partner
            action = action
            label = label
            payload = payload
            assertion = chunkedAssertions
        }

    let convertEdge (transitions : TransitionMap) (fromState, toState) attributes =
        let processAttribute transitions attribute =
            let fromState = int fromState
            let toState = int toState
            let label = Map.find "label" attribute
            let transition = parseTransition fromState toState label
            addTransition transitions transition
        List.fold processAttribute transitions attributes

    let convert (graph: GraphData.GraphData) (recursiveRefinement: bool) : CFSM =
        newSyntax := recursiveRefinement
        let edges = graph.Edges
        let nodes = graph.Nodes |> Map.toList |> List.map (fst >> int)
        let init = List.min nodes
        let initMap = List.map (fun node -> node, []) nodes |> Map.ofList
        let transitionMap = Map.fold convertEdge initMap edges
        let finals = Map.filter (fun _ trans -> List.isEmpty trans) transitionMap |> Map.toList |> List.map fst
        init, finals, transitionMap
