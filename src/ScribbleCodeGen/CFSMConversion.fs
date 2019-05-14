namespace ScribbleCodeGen

open DotParser

module CFSMConversion =

    let newTransitionMap = Map.empty

    let allStates : CFSM -> State list = snd >> Map.toList >> List.map (fst >> int)

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

    let convert (graph: GraphData.GraphData) protocol localRole : CFSM =
        let edges = graph.Edges
        let init = graph.Nodes |> Map.toList |> List.map (fst >> int) |> List.min
        let transitionMap = Map.fold convertEdge newTransitionMap edges
        init, transitionMap
