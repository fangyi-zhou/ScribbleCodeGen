namespace ScribbleCodeGen

open System.Text.RegularExpressions

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

    let parseOldAssertionString str : string * char seq =
        match Seq.tryHead str with
        | Some '@' ->
            let str = Seq.tail str
            match Seq.head str with
            | '\"' ->
                let str = Seq.tail str
                let assertion, rest = span ((<>) '\"') str
                let rest =
                    match Seq.head rest with
                    | '\"' -> Seq.tail rest
                    | _ -> failwith "unfinished assertion, missing '\"'"
                seqToString assertion, rest
            | _ -> failwith "invalid assertion, missing '\"'"
        | Some _ -> failwithf "unknown assertion %s" (seqToString str)
        | None ->
            (* No assertion *)
            "", str

    let fixAssertionDiscrepancy assertions =
        let assertions = Regex.Replace(assertions, @"\bTrue\b", "true")
        let assertions = Regex.Replace(assertions, @"\bFalse\b", "false")
        (* "true" means no assertions *)
        if assertions = "true" then "" else assertions

    let parseNewAssertionString str : string * char seq =
        let assertions, rest = span ((<>) '}') str
        let assertions =
            match Seq.tryHead assertions with
            | Some '{' ->
                let assertions = Seq.tail assertions |> seqToString
                fixAssertionDiscrepancy assertions
            | _ -> failwith "invalid assertion, missing '{'"
        let rest =
            match Seq.tryHead rest with
            | Some '}' -> Seq.tail rest
            | _ -> failwith "unexpected"
        assertions, rest

    let parseRecVars str : char seq list * char seq =
        match Seq.tryHead str with
        | Some '<' ->
            let rec aux str acc =
                let str = skipSpaces str
                let expr, rest = span (fun c -> c <> '>' && c <> ',') str
                match Seq.tryHead rest with
                | Some '>' ->
                    let acc = if Seq.isEmpty expr then acc else expr :: acc
                    List.rev acc, Seq.tail rest
                | Some ',' -> aux (Seq.tail rest) (expr :: acc)
                | _ -> failwith "Unexpected recursion expression"
            aux (Seq.tail str) []
        | _ -> failwith "invalid recursion variable list, missing '<'"


    let parseDotLabelPrefix (str: string) : Role * Action * Label * Payload * char seq =
        let str = str.ToCharArray() |> Seq.ofArray
        let partner, str = parseRole str
        let action, str = parseAction str
        let label, str = parseLabel str
        let payload, str = parsePayload str
        partner, action, label, payload, str

    let parseOldDotLabel (str: string) =
        let partner, action, label, payload, str = parseDotLabelPrefix str
        let assertion, str = parseOldAssertionString str
        if not (Seq.isEmpty str) then eprintfn "Unexpected %s" (seqToString str)
        partner, action, label, payload, assertion, []

    let parseNewDotLabel (str: string) =
        let partner, action, label, payload, str = parseDotLabelPrefix str
        let assertion, str = parseNewAssertionString str
        let stateVars, str = parseRecVars str
        let stateVars = List.map seqToString stateVars
        if not (Seq.isEmpty str) then eprintfn "Unexpected %s" (seqToString str)
        partner, action, label, payload, assertion, stateVars

    let parseRecVarEntry (str: string) =
        let str = Seq.skipWhile ((<>) '<') str
        match Seq.tryHead str with
        | Some '<' ->
            let parseSingle str =
                let var, rest = span ((<>) ':') str
                match Seq.tryHead rest with
                | Some ':' ->
                    let rest = Seq.tail rest
                    match Seq.tryHead rest with
                    | Some '=' ->
                        let rest = Seq.tail rest |> skipSpaces
                        seqToString var, seqToString rest
                    | _ -> failwith "invalid initial expression, missing '='"
                | _ -> failwith "invalid initial expression, missing ':'"
            let rec aux str acc =
                let str = skipSpaces str
                let expr, rest = span (fun c -> c <> '>' && c <> ',') str
                match Seq.tryHead rest with
                | Some '>' ->
                    let acc = if Seq.isEmpty expr then acc else (parseSingle expr) :: acc
                    let assertions = Seq.tail rest |> skipSpaces |> seqToString |> fixAssertionDiscrepancy
                    List.rev acc, assertions
                | Some ',' -> aux (Seq.tail rest) ((parseSingle expr) :: acc)
                | _ -> failwith "Unexpected recursion expression"
            aux (Seq.tail str) []
        | _ -> failwith "invalid recursion variable list, missing '<'"
