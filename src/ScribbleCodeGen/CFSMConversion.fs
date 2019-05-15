namespace ScribbleCodeGen

module CFSMConversion =

    let newTransitionMap = Map.empty

    let addTransition (transitions: TransitionMap) (transition: Transition) : TransitionMap =
        let from = transition.fromState
        match Map.tryFind from transitions with
        | Some oldTransitions -> Map.add from (transition :: oldTransitions) transitions
        | None -> Map.add from [transition] transitions

    let parseTransition fromState toState label : Transition =
        let partner, action, label, payload, assertion = Parsing.parseDotLabel label
        {
            fromState = fromState
            toState = toState
            partner = partner
            action = action
            label = label
            payload = payload
            assertion = assertion
        }

    let convertEdge (transitions : TransitionMap) (fromState, toState) attributes =
        let attribute = List.head attributes
        let fromState = int fromState
        let toState = int toState
        let label = Map.find "label" attribute
        let transition = parseTransition fromState toState label
        addTransition transitions transition

    let convert (graph: GraphData.GraphData) : CFSM =
        let edges = graph.Edges
        let nodes = graph.Nodes |> Map.toList |> List.map (fst >> int)
        let init = List.min nodes
        let initMap = List.map (fun node -> node, []) nodes |> Map.ofList
        let transitionMap = Map.fold convertEdge initMap edges
        init, transitionMap
