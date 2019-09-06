namespace ScribbleCodeGen

open CodeGenVanilla
open CodeGenEventStyle

module CodeGen =
    let generateCodeContent (cfsm : CFSM) stateVarMap codeGenMode localRole =
        match codeGenMode with
        | LegacyApi -> generateCodeContentVanillaApi cfsm
        | EventApi | FStar -> generateCodeContentEventStyleApi cfsm stateVarMap localRole
