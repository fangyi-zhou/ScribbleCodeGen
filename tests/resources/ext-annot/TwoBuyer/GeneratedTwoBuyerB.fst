module GeneratedTwoBuyerB
(* This file is GENERATED, do not modify manually *)
open FStar.All
open FStar.Error

type role =
    | A
    | S
type state33Choice =
    | Choice33ok
    | Choice33no
and state33_no = {
    _dumstate33_no : unit;
    y : int;
    a : (a : int{((a) >= (0))});
}
and state33_ok = {
    _dumstate33_ok : unit;
    y : int;
    a : (a : int{((a) >= (0)) && (((y) - (a)) <= (a))});
}
type state30 = unit
and state31 = unit
and state32 = {
    _dumstate32 : unit;
    y : int;
}
and state33 = {
    _dumstate33 : unit;
    y : int;
    a : (a : int{((a) >= (0))});
}
noeq type callbacksB = {
    state30OnreceivequoteB : (st: state30) -> (y: int) -> ML (unit);
    state32OnreceiveproposeA : (st: state32) -> (a: int{((a) >= (0))}) -> ML (unit);
    state33 : (st: state33) -> ML (choice:state33Choice{((((choice) = (Choice33ok)) && (((Mkstate33?.y st) - (Mkstate33?.a st)) <= (Mkstate33?.a st))) || ((choice) = (Choice33no)))});
    state33Onsendok : (st: state33_ok) -> ML (b:int{((b) = ((Mkstate33_ok?.y st) - (Mkstate33_ok?.a st))) && (((Mkstate33_ok?.y st) - (Mkstate33_ok?.a st)) <= (Mkstate33_ok?.a st))});
    state33Onsendno : (st: state33_no) -> ML (unit);
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
    let rec runState30 (st: state30) : ML unit =
        let label = comms.recv_string S () in
        match label with
            | "quoteB" ->
                let y = comms.recv_int S () in
                callbacks.state30OnreceivequoteB st y;
                let st : state32 = {
                    _dumstate32 = ();
                    y = y;
                }
                in
                runState32 st
            | _ -> unexpected "unexpected label"
    and runState31 (st: state31) : ML unit =
        ()
    and runState32 (st: state32) : ML unit =
        let label = comms.recv_string A () in
        match label with
            | "proposeA" ->
                let a = comms.recv_int A () in
                assume (((a) >= (0)));
                callbacks.state32OnreceiveproposeA st a;
                let st : state33 = {
                    _dumstate33 = ();
                    y = (Mkstate32?.y st);
                    a = a;
                }
                in
                runState33 st
            | _ -> unexpected "unexpected label"
    and runState33 (st: state33) : ML unit =
        let label = callbacks.state33 st in
        match label with
            | Choice33ok ->
                let st : state33_ok = {
                    _dumstate33_ok = ();
                    y = (Mkstate33?.y st);
                    a = (Mkstate33?.a st);
                }
                in
                comms.send_string A "ok";
                let b = callbacks.state33Onsendok st in
                comms.send_int A b;
                let st : state31 = ()
                in
                runState31 st
            | Choice33no ->
                let st : state33_no = {
                    _dumstate33_no = ();
                    y = (Mkstate33?.y st);
                    a = (Mkstate33?.a st);
                }
                in
                comms.send_string A "no";
                let _dum2 = callbacks.state33Onsendno st in
                comms.send_unit A _dum2;
                let st : state31 = ()
                in
                runState31 st
    in
    let initState : state30 =
        ()
    in
    runState30 initState
