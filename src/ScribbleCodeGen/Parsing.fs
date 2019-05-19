namespace ScribbleCodeGen

module Parsing =

    let isVariable x
        = System.Char.IsLetter x || System.Char.IsDigit x || x = '_'

    let span f xs =
        Seq.takeWhile f xs, Seq.skipWhile f xs

    let skipSpaces str = Seq.skipWhile (System.Char.IsWhiteSpace) str

    let seqToString str = new string(Array.ofSeq str)

    let parseLabel str : Label * char seq =
        let label, rest = span isVariable str
        seqToString label, rest

    let rec parsePayloadItems str : Payload * char seq =
        let str = skipSpaces str
        if Seq.isEmpty str then [], str
        else
            let variable, rest = span isVariable str
            let rest = skipSpaces rest
            let rest =
                match Seq.head rest with
                | ':' -> Seq.tail rest
                | _ -> failwith "expected ':' in payload"
            let rest = skipSpaces rest
            let ty, rest = span isVariable rest
            let payloadItem = seqToString variable, seqToString ty
            let rest = skipSpaces rest
            match Seq.tryHead rest with
            | Some ',' ->
                let rest = Seq.tail rest
                let restPayloadItems, rest = parsePayloadItems rest
                (payloadItem :: restPayloadItems), rest
            | Some _ ->
                failwithf "unexpected item in payloads %s" (seqToString rest)
            | None ->
                [payloadItem], rest

    let parsePayload str : Payload * char seq =
        match Seq.head str with
        | '(' ->
            let str = Seq.skip 1 str
            let items, rest = span ((<>) ')') str
            let payload, rest' = parsePayloadItems items
            if not (Seq.isEmpty rest') then eprintfn "LeftOver Payloads %s" (seqToString rest')
            let rest =
                match Seq.head rest with
                | ')' -> Seq.tail rest
                | _ -> failwith "unfinished payload, missing ')'"
            payload, rest
        | _ -> failwith "invalid payload"

    let parseRole str : Role * char seq =
        let role, rest = span isVariable str
        seqToString role, rest

    let parseAction str : Action * char seq =
        match Seq.head str with
        | '?' ->
            let str = Seq.skip 1 str
            match Seq.head str with
            | '?' -> Accept, Seq.tail str
            | _ -> Receive, str
        | '!' ->
            let str = Seq.skip 1 str
            match Seq.head str with
            | '!' -> Request, Seq.tail str
            | _ -> Send, str
        | _ -> failwith "invalid action"

    let parseAssertionString str : string * char seq =
        (* dollar is used to wrap the assertion instead of quotes due to DotParser issue *)
        match Seq.tryHead str with
        | Some '@' ->
            let str = Seq.tail str
            match Seq.head str with
            | '$' ->
                let str = Seq.tail str
                let assertion, rest = span ((<>) '$') str
                let rest =
                    match Seq.head rest with
                    | '$' -> Seq.tail rest
                    | _ -> failwith "unfinished assertion, missing '$'"
                seqToString assertion, rest
            | _ -> failwith "invalid assertion, missing '$'"
        | Some _ -> failwithf "unknown assertion %s" (seqToString str)
        | None ->
            (* No assertion *)
            "", str

    let parseDotLabel (str: string) : Role * Action * Label * Payload * string =
        let str = str.ToCharArray() |> Seq.ofArray
        let partner, str = parseRole str
        let action, str = parseAction str
        let label, str = parseLabel str
        let payload, str = parsePayload str
        let assertion, str = parseAssertionString str
        if not (Seq.isEmpty str) then eprintfn "Unexpected %s" (seqToString str)
        partner, action, label, payload, assertion
