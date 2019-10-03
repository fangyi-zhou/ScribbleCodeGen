module GeneratedAdderS
(* This file is GENERATED, do not modify manually *)
open FStar.All
open FStar.Error

type role =
    | C
type state20 = unit
and state21 = unit
and state22 = {
    _dumstate22 : unit;
    u : int;
}
and state23 = {
    _dumstate23 : unit;
    u : int;
    w : int;
}
and state24 = {
    _dumstate24 : unit;
    u : int;
    w : int;
    v : int;
}
and state25 = {
    _dumstate25 : unit;
    u : int;
}
noeq type callbacksS = {
    state20OnreceiveHELLO : (st: state20) -> (u: int) -> ML (unit);
    state22OnreceiveBYE : (st: state22) -> ML (unit);
    state22OnreceiveADD : (st: state22) -> (w: int) -> ML (unit);
    state23OnreceiveADD : (st: state23) -> (v: int) -> ML (unit);
    state24OnsendRES : (st: state24) -> ML (int);
    state25OnsendBYE : (st: state25) -> ML (unit);
}
noeq type communications = {
    send_int : role -> int -> ML unit;
    send_string : role -> string -> ML unit;
    send_unit : role -> unit -> ML unit;
    recv_int : role -> unit -> ML int;
    recv_string : role -> unit -> ML string;
    recv_unit : role -> unit -> ML unit;
}
let run (callbacks : callbacksS) (comms : communications) : ML unit =
    let rec runState20 (st: state20) : ML unit =
        let label = comms.recv_string C () in
        match label with
            | "HELLO" ->
                let u = comms.recv_int C () in
                callbacks.state20OnreceiveHELLO st u;
                let st : state22 = {
                    _dumstate22 = ();
                    u = u;
                }
                in
                runState22 st
            | _ -> unexpected "unexpected label"
    and runState21 (st: state21) : ML unit =
        ()
    and runState22 (st: state22) : ML unit =
        let label = comms.recv_string C () in
        match label with
            | "BYE" ->
                let _ = comms.recv_unit C () in
                callbacks.state22OnreceiveBYE st ;
                let st : state25 = {
                    _dumstate25 = ();
                    u = (Mkstate22?.u st);
                }
                in
                runState25 st
            | "ADD" ->
                let w = comms.recv_int C () in
                callbacks.state22OnreceiveADD st w;
                let st : state23 = {
                    _dumstate23 = ();
                    u = (Mkstate22?.u st);
                    w = w;
                }
                in
                runState23 st
            | _ -> unexpected "unexpected label"
    and runState23 (st: state23) : ML unit =
        let label = comms.recv_string C () in
        match label with
            | "ADD" ->
                let v = comms.recv_int C () in
                callbacks.state23OnreceiveADD st v;
                let st : state24 = {
                    _dumstate24 = ();
                    u = (Mkstate23?.u st);
                    w = (Mkstate23?.w st);
                    v = v;
                }
                in
                runState24 st
            | _ -> unexpected "unexpected label"
    and runState24 (st: state24) : ML unit =
        comms.send_string C "RES";
        let f = callbacks.state24OnsendRES st in
        comms.send_int C f;
        let st : state20 = ()
        in
        runState20 st
    and runState25 (st: state25) : ML unit =
        comms.send_string C "BYE";
        let _ = callbacks.state25OnsendBYE st in
        comms.send_unit C _;
        let st : state21 = ()
        in
        runState21 st
    in
    runState20 ()
