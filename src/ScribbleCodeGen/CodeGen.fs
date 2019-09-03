namespace ScribbleCodeGen

open CodeGenVanilla
open CodeGenEventStyle

module CodeGen =
    let generateCodeContent (cfsm : CFSM) stateVarMap legacyApi localRole =
        if legacyApi
            then generateCodeContentVanillaApi cfsm
            else generateCodeContentEventStyleApi cfsm stateVarMap localRole
