namespace ScribbleCodeGen

open DotParser

module Library =

    let processScribbleOutput content protocol localRole =
        let parsed = parse content
        let cfsm = CFSMConversion.convert parsed
        CodePrinter.generateCode cfsm protocol localRole
