namespace ScribbleCodeGen

open CodeGenCommon
open CodeGenVanilla
open CodeGenEventStyle

module CodeGen =
    let generateCodeContent (cfsm : CFSM) stateVarMap localRole =
        match !codeGenMode with
        | LegacyApi -> generateCodeContentVanillaApi cfsm
        | EventApi | FStar -> generateCodeContentEventStyleApi cfsm stateVarMap localRole
