namespace ScribbleCodeGen

module CFSMAnalysis =

    let constructVariableMap (cfsm : CFSM) : StateVariableMap =
        let init, allTransitions = cfsm
        let rec aux varMap state vars =
            if Map.containsKey state varMap
            then varMap
            else
                let varMap = Map.add state vars varMap
                let transitions = Map.find state allTransitions
                let addVariablesFromTransition transition =
                    let fromState = transition.fromState
                    let fromStateVars = Map.find fromState varMap
                    let payloads = transition.payload
                    List.append fromStateVars payloads
                List.fold (fun varMap transition -> aux varMap transition.toState (addVariablesFromTransition transition)) varMap transitions
        aux Map.empty init []
