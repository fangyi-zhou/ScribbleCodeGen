namespace ScribbleCodeGen

open CodeGenVanilla
open CodeGenEventStyle

module CodeGen =
    let generateCodeContent (cfsm : CFSM) eventStyleApi =
        if eventStyleApi
            then generateCodeContentEventStyleApi cfsm
            else generateCodeContentVanillaApi cfsm
