namespace ScribbleCodeGen

module CodeGenCommon =

    type Method = string
    type Member = string
    type UnionCase = string
    type Field = string
    type FieldType = string
    type Refinement = string

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

    let mkStateName state =
        sprintf "State%d" state

    let allRoles ((_, transitions) : CFSM) =
        let accumRoles roles _ transitions =
            let newRoles = List.map (fun (t: Transition) -> t.partner) transitions
            Set.union (Set.ofList newRoles) roles
        Map.fold accumRoles Set.empty transitions

    let allStates : CFSM -> State list = snd >> Map.toList >> List.map (fst >> int)

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

    let productOfPayload payload =
        if List.isEmpty payload
        then "unit"
        else
            let getType (_, tyName) =
                match Map.tryFind tyName defaultTypeAliasMap with
                | Some ty -> ty
                | None -> tyName
            List.map getType payload |> Seq.ofList |> String.concat " * "

    let cleanUpVarMap stateVarMap =
        let cleanUpSingle _ =
            List.filter (fun (name, _, _) -> not (isDummy name))
        Map.map cleanUpSingle stateVarMap

    let addRole content role =
        let roleUnion = Union [role]
        Map.add role roleUnion content
