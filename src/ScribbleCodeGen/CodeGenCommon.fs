namespace ScribbleCodeGen

module CodeGenCommon =

    type Method = string
    type Member = string
    type Field = string
    type FieldType = string
    type Refinement = string
    type Tag = string
    type UnionCase = Tag * FieldType list * Refinement option

    type Object = {
        methods : Method list
        members : Member list
    }

    type RecordItem = Field * FieldType * Refinement option

    type TypeDef =
        | Object of Object
        | Union of UnionCase list
        | Record of RecordItem list

    type Content = Map<string, TypeDef>

    let newObject = Object {
        methods = []
        members = []
    }

    let defaultTypeAliasMap = Map.ofList [
        "int", "int";
        "string", "string";
        "_Unit", "unit";
    ]

    let resolveTypeAlias tyName =
        match Map.tryFind tyName defaultTypeAliasMap with
        | Some ty -> ty
        | None -> tyName

    let mkStateName state =
        sprintf "State%d" state

    let allRoles ((_, _, transitions) : CFSM) =
        let accumRoles roles _ transitions =
            let newRoles = List.map (fun (t: Transition) -> t.partner) transitions
            Set.union (Set.ofList newRoles) roles
        Map.fold accumRoles Set.empty transitions

    let allStates : CFSM -> State list = (fun (_, _, v) -> v) >> Map.toList >> List.map (fst >> int)

    let isDummy (x : string) = x.StartsWith("_")

    let convertAction action =
        match action with
        | Send -> "send"
        | Receive -> "receive"
        | Accept -> "accept"
        | Request -> "request"

    let stateHasExternalChoice transitions =
        let receiveCount = List.filter (fun t -> t.action = Receive) >> List.length
        receiveCount transitions > 1

    let stateHasInternalChoice transitions =
        let receiveCount = List.filter (fun t -> t.action = Send) >> List.length
        receiveCount transitions > 1

    let productOfPayload payload =
        if List.isEmpty payload
        then "unit"
        else
            let getType (_, tyName) = resolveTypeAlias tyName
            List.map getType payload |> Seq.ofList |> String.concat " * "

    let productOfRefinedPayload payload =
        if List.isEmpty payload
        then "unit"
        else
            let getType (_, tyName, refinement) =
                match refinement with
                | Some r -> r
                | None -> tyName
            List.map getType payload |> Seq.ofList |> String.concat " * "

    let curriedPayload payload =
        if List.isEmpty payload
        then "unit"
        else
            let getType (_, tyName) = resolveTypeAlias tyName
            List.map getType payload |> Seq.ofList |> String.concat " -> "

    let curriedPayloadRefined payload =
        if List.isEmpty payload
        then "unit"
        else
            let getType (var, tyName, refinement) =
                let refinement =
                    match refinement with
                    | Some r -> r
                    | None -> tyName
                sprintf "(%s : %s)" var refinement
            List.map getType payload |> Seq.ofList |> String.concat " -> "

    let cleanUpVarMap stateVarMap =
        let cleanUpSingle _ (vars, assertions) =
            List.filter (fun (name, _) -> not (isDummy name)) vars, assertions
        Map.map cleanUpSingle stateVarMap

    let addRole content role =
        let roleUnion = Union [role, [], None]
        Map.add role roleUnion content
