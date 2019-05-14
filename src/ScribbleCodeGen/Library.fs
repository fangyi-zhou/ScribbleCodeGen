namespace ScribbleCodeGen

open DotParser

module Library =

    let moduleName = "ScribbleGenerated"

    let parseScribbleOutput (content: string) =
        let parsed = parse content
        printfn "%A" parsed
        ()
