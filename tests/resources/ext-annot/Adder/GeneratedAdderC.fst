module GeneratedAdderC
(* This file is GENERATED, do not modify manually *)
open FStar.All
open FStar.Error

type role =
    | S
type state10Choice =
    | Choice10BYE
    | Choice10ADD
and state10_ADD = {
    _dumstate10_ADD : unit;
    u : int;
}
and state10_BYE = {
    _dumstate10_BYE : unit;
    u : int;
}
type state10 = {
    _dumstate10 : unit;
    u : int;
}
and state11 = {
    _dumstate11 : unit;
    u : int;
    x : int;
}
and state12 = {
    _dumstate12 : unit;
    u : int;
    x : int;
    y : int;
}
and state13 = {
    _dumstate13 : unit;
    u : int;
}
and state8 = unit
and state9 = unit
noeq type callbacksC = {
    state8OnsendHELLO : (st: state8) -> ML (int);
    state10 : (st: state10) -> ML (choice:state10Choice{(((choice) = (Choice10BYE)) || ((choice) = (Choice10ADD)))});
    state10OnsendBYE : (st: state10_BYE) -> ML (unit);
    state10OnsendADD : (st: state10_ADD) -> ML (int);
    state11OnsendADD : (st: state11) -> ML (int);
    state12OnreceiveRES : (st: state12) -> (z: int{((z) = ((Mkstate12?.x st) + (Mkstate12?.y st)))}) -> ML (unit);
    state13OnreceiveBYE : (st: state13) -> ML (unit);
}
noeq type communications = {
    send_int : role -> int -> ML unit;
    send_string : role -> string -> ML unit;
    send_unit : role -> unit -> ML unit;
    recv_int : role -> unit -> ML int;
    recv_string : role -> unit -> ML string;
    recv_unit : role -> unit -> ML unit;
}
let run (callbacks : callbacksC) (comms : communications) : ML unit =
    let rec runState8 (st: state8) : ML unit =
        comms.send_string S "HELLO";
        let u = callbacks.state8OnsendHELLO st in
        comms.send_int S u;
        let st : state10 = {
            _dumstate10 = ();
            u = u;
        }
        in
        runState10 st
    and runState9 (st: state9) : ML unit =
        ()
    and runState10 (st: state10) : ML unit =
        let label = callbacks.state10 st in
        match label with
            | Choice10BYE ->
                let st : state10_BYE = {
                    _dumstate10_BYE = ();
                    u = (Mkstate10?.u st);
                }
                in
                comms.send_string S "BYE";
                let _dum1 = callbacks.state10OnsendBYE st in
                comms.send_unit S _dum1;
                let st : state13 = {
                    _dumstate13 = ();
                    u = (Mkstate10_BYE?.u st);
                }
                in
                runState13 st
            | Choice10ADD ->
                let st : state10_ADD = {
                    _dumstate10_ADD = ();
                    u = (Mkstate10?.u st);
                }
                in
                comms.send_string S "ADD";
                let x = callbacks.state10OnsendADD st in
                comms.send_int S x;
                let st : state11 = {
                    _dumstate11 = ();
                    u = (Mkstate10_ADD?.u st);
                    x = x;
                }
                in
                runState11 st
    and runState11 (st: state11) : ML unit =
        comms.send_string S "ADD";
        let y = callbacks.state11OnsendADD st in
        comms.send_int S y;
        let st : state12 = {
            _dumstate12 = ();
            u = (Mkstate11?.u st);
            x = (Mkstate11?.x st);
            y = y;
        }
        in
        runState12 st
    and runState12 (st: state12) : ML unit =
        let label = comms.recv_string S () in
        match label with
            | "RES" ->
                let z = comms.recv_int S () in
                assume (((z) = ((Mkstate12?.x st) + (Mkstate12?.y st))));
                callbacks.state12OnreceiveRES st z;
                let st : state8 = ()
                in
                runState8 st
            | _ -> unexpected "unexpected label"
    and runState13 (st: state13) : ML unit =
        let label = comms.recv_string S () in
        match label with
            | "BYE" ->
                let _dum2 = comms.recv_unit S () in
                callbacks.state13OnreceiveBYE st ;
                let st : state9 = ()
                in
                runState9 st
            | _ -> unexpected "unexpected label"
    in
    runState8 ()
