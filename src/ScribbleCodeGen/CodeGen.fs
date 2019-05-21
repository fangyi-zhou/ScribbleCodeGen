namespace ScribbleCodeGen

open CodeGenVanilla
open CodeGenEventStyle

module CodeGen =
    let generateCodeContent (cfsm : CFSM) eventStyleApi localRole =
        if eventStyleApi
            then generateCodeContentEventStyleApi cfsm localRole
            else generateCodeContentVanillaApi cfsm
