namespace ScribbleCodeGen

open CodeGenVanilla
open CodeGenEventStyle

module CodeGen =
    let generateCodeContent (cfsm : CFSM) legacyApi localRole =
        if legacyApi
            then generateCodeContentVanillaApi cfsm
            else generateCodeContentEventStyleApi cfsm localRole
