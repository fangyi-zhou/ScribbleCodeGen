namespace ScribbleCodeGen

open DotParser

module Library =

    let moduleName = "ScribbleGenerated"

    let parseScribbleOutput content protocol localRole =
        let parsed = parse content
        let cfsm = CFSMConversion.convert parsed protocol localRole
        printfn "parsed: %A" parsed
        printfn "CFSM: %A" cfsm
        ()
