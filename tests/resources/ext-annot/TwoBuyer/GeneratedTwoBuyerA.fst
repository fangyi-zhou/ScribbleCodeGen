module GeneratedTwoBuyerA
(* This file is GENERATED, do not modify manually *)
open FStar.All
open FStar.Error

type role =
    | B
    | S
type state19 = unit
and state20 = unit
and state21 = {
    _dumstate21 : unit;
    id : int;
}
and state22 = {
    _dumstate22 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
}
and state23 = {
    _dumstate23 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
    a : (a : int{((a) >= (0)) && ((a) <= (x))});
}
and state24 = {
    _dumstate24 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
    a : (a : int{((a) >= (0)) && ((a) <= (x))});
}
and state25 = {
    _dumstate25 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
    a : (a : int{((a) >= (0)) && ((a) <= (x))});
    b : (b : int{((b) <= (a))});
}
noeq type callbacksA = {
    state19OnsendbookId : (st: state19) -> ML (int);
    state21OnreceivequoteA : (st: state21) -> (x: int{((x) >= (0))}) -> ML (unit);
    state22OnsendproposeA : (st: state22) -> ML (a:int{((a) >= (0)) && ((a) <= (Mkstate22?.x st))});
    state23Onreceiveok : (st: state23) -> (b: int{((b) <= (Mkstate23?.a st))}) -> ML (unit);
    state23Onreceiveno : (st: state23) -> ML (unit);
    state24Onsendcancel : (st: state24) -> ML (unit);
    state25Onsendbuy : (st: state25) -> ML (unit);
}
noeq type communications = {
    send_int : role -> int -> ML unit;
    send_string : role -> string -> ML unit;
    send_unit : role -> unit -> ML unit;
    recv_int : role -> unit -> ML int;
    recv_string : role -> unit -> ML string;
    recv_unit : role -> unit -> ML unit;
}
let run (callbacks : callbacksA) (comms : communications) : ML unit =
    let rec runState19 (st: state19) : ML unit =
        comms.send_string S "bookId";
        let id = callbacks.state19OnsendbookId st in
        comms.send_int S id;
        let st : state21 = {
            _dumstate21 = ();
            id = id;
        }
        in
        runState21 st
    and runState20 (st: state20) : ML unit =
        ()
    and runState21 (st: state21) : ML unit =
        let label = comms.recv_string S () in
        match label with
            | "quoteA" ->
                let x = comms.recv_int S () in
                assume (((x) >= (0)));
                callbacks.state21OnreceivequoteA st x;
                let st : state22 = {
                    _dumstate22 = ();
                    id = (Mkstate21?.id st);
                    x = x;
                }
                in
                runState22 st
            | _ -> unexpected "unexpected label"
    and runState22 (st: state22) : ML unit =
        comms.send_string B "proposeA";
        let a = callbacks.state22OnsendproposeA st in
        comms.send_int B a;
        let st : state23 = {
            _dumstate23 = ();
            id = (Mkstate22?.id st);
            x = (Mkstate22?.x st);
            a = a;
        }
        in
        runState23 st
    and runState23 (st: state23) : ML unit =
        let label = comms.recv_string B () in
        match label with
            | "ok" ->
                let b = comms.recv_int B () in
                assume (((b) <= (Mkstate23?.a st)));
                callbacks.state23Onreceiveok st b;
                let st : state25 = {
                    _dumstate25 = ();
                    id = (Mkstate23?.id st);
                    x = (Mkstate23?.x st);
                    a = (Mkstate23?.a st);
                    b = b;
                }
                in
                runState25 st
            | "no" ->
                let _dum2 = comms.recv_unit B () in
                callbacks.state23Onreceiveno st ;
                let st : state24 = {
                    _dumstate24 = ();
                    id = (Mkstate23?.id st);
                    x = (Mkstate23?.x st);
                    a = (Mkstate23?.a st);
                }
                in
                runState24 st
            | _ -> unexpected "unexpected label"
    and runState24 (st: state24) : ML unit =
        comms.send_string S "cancel";
        let _dum3 = callbacks.state24Onsendcancel st in
        comms.send_unit S _dum3;
        let st : state20 = ()
        in
        runState20 st
    and runState25 (st: state25) : ML unit =
        comms.send_string S "buy";
        let _dum1 = callbacks.state25Onsendbuy st in
        comms.send_unit S _dum1;
        let st : state20 = ()
        in
        runState20 st
    in
    let initState : state19 =
        ()
    in
    runState19 initState
