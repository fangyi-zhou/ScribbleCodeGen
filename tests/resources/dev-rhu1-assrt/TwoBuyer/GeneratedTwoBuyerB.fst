module GeneratedTwoBuyerB
(* This file is GENERATED, do not modify manually *)
open FStar.All
open FStar.Error

type role =
    | A
    | S
type state26Choice =
    | Choice26no
    | Choice26ok
and state26_no = {
    _dumstate26_no : unit;
    y : int;
    a : (a : int{((a) >= (0))});
}
and state26_ok = {
    _dumstate26_ok : unit;
    y : int;
    a : (a : int{((a) >= (0)) && (((y) - (a)) <= (a))});
}
type state23 = unit
and state24 = unit
and state25 = {
    _dumstate25 : unit;
    y : int;
}
and state26 = {
    _dumstate26 : unit;
    y : int;
    a : (a : int{((a) >= (0))});
}
noeq type callbacksB = {
    state23OnreceivequoteB : (st: state23) -> (y: int) -> ML (unit);
    state25OnreceiveproposeA : (st: state25) -> (a: int{((a) >= (0))}) -> ML (unit);
    state26 : (st: state26) -> ML (choice:state26Choice{(((choice) = (Choice26no)) || (((choice) = (Choice26ok)) && (((Mkstate26?.y st) - (Mkstate26?.a st)) <= (Mkstate26?.a st))))});
    state26Onsendno : (st: state26_no) -> ML (unit);
    state26Onsendok : (st: state26_ok) -> ML (b:int{((b) = ((Mkstate26_ok?.y st) - (Mkstate26_ok?.a st))) && (((Mkstate26_ok?.y st) - (Mkstate26_ok?.a st)) <= (Mkstate26_ok?.a st))});
}
noeq type communications = {
    send_int : role -> int -> ML unit;
    send_string : role -> string -> ML unit;
    send_unit : role -> unit -> ML unit;
    recv_int : role -> unit -> ML int;
    recv_string : role -> unit -> ML string;
    recv_unit : role -> unit -> ML unit;
}
let run (callbacks : callbacksB) (comms : communications) : ML unit =
    let rec runState23 (st: state23) : ML unit =
        let label = comms.recv_string S () in
        match label with
            | "quoteB" ->
                let y = comms.recv_int S () in
                callbacks.state23OnreceivequoteB st y;
                let st : state25 = {
                    _dumstate25 = ();
                    y = y;
                }
                in
                runState25 st
            | _ -> unexpected "unexpected label"
    and runState24 (st: state24) : ML unit =
        ()
    and runState25 (st: state25) : ML unit =
        let label = comms.recv_string A () in
        match label with
            | "proposeA" ->
                let a = comms.recv_int A () in
                assume (((a) >= (0)));
                callbacks.state25OnreceiveproposeA st a;
                let st : state26 = {
                    _dumstate26 = ();
                    y = (Mkstate25?.y st);
                    a = a;
                }
                in
                runState26 st
            | _ -> unexpected "unexpected label"
    and runState26 (st: state26) : ML unit =
        let label = callbacks.state26 st in
        match label with
            | Choice26no ->
                let st : state26_no = {
                    _dumstate26_no = ();
                    y = (Mkstate26?.y st);
                    a = (Mkstate26?.a st);
                }
                in
                comms.send_string A "no";
                let _dummy = callbacks.state26Onsendno st in
                comms.send_unit A _dummy;
                let st : state24 = ()
                in
                runState24 st
            | Choice26ok ->
                let st : state26_ok = {
                    _dumstate26_ok = ();
                    y = (Mkstate26?.y st);
                    a = (Mkstate26?.a st);
                }
                in
                comms.send_string A "ok";
                let b = callbacks.state26Onsendok st in
                comms.send_int A b;
                let st : state24 = ()
                in
                runState24 st
    in
    runState23 ()
