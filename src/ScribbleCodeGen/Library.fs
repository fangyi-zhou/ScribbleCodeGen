namespace ScribbleCodeGen

open DotParser

module Library =
    let processScribbleOutput content protocol localRole codeGenMode =
        let parsed = parse content
        let cfsm = CFSMConversion.convert parsed
        CodeGenCommon.codeGenMode := codeGenMode
        CodePrinter.generateCode cfsm protocol localRole
