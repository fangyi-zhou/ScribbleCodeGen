namespace ScribbleCodeGen

open DotParser

module Library =
    let processScribbleOutput content protocol localRole codeGenMode recursiveRefinement =
        let parsed = parse content
        let cfsm = CFSMConversion.convert parsed recursiveRefinement
        CodeGenCommon.codeGenMode := codeGenMode
        CodePrinter.generateCode cfsm protocol localRole
