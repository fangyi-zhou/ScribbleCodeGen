module GeneratedTwoBuyerA
(* This file is GENERATED, do not modify manually *)
open FStar.All
open FStar.Error

type role =
    | B
    | S
type state10 = unit
and state11 = {
    _dumstate11 : unit;
    id : int;
}
and state12 = {
    _dumstate12 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
}
and state13 = {
    _dumstate13 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
    a : (a : int{((a) >= (0)) && ((a) <= (x))});
}
and state14 = {
    _dumstate14 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
    a : (a : int{((a) >= (0)) && ((a) <= (x))});
}
and state15 = {
    _dumstate15 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
    a : (a : int{((a) >= (0)) && ((a) <= (x))});
    b : int;
}
and state9 = unit
noeq type callbacksA = {
    state9OnsendbookId : (st: state9) -> ML (int);
    state11OnreceivequoteA : (st: state11) -> (x: int{((x) >= (0))}) -> ML (unit);
    state12OnsendproposeA : (st: state12) -> ML (a:int{((a) >= (0)) && ((a) <= (Mkstate12?.x st))});
    state13Onreceiveok : (st: state13) -> (b: int) -> ML (unit);
    state13Onreceiveno : (st: state13) -> ML (unit);
    state14Onsendcancel : (st: state14) -> ML (unit);
    state15Onsendbuy : (st: state15) -> ML (unit);
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
    let rec runState9 (st: state9) : ML unit =
        comms.send_string S "bookId";
        let id = callbacks.state9OnsendbookId st in
        comms.send_int S id;
        let st : state11 = {
            _dumstate11 = ();
            id = id;
        }
        in
        runState11 st
    and runState10 (st: state10) : ML unit =
        ()
    and runState11 (st: state11) : ML unit =
        let label = comms.recv_string S () in
        match label with
            | "quoteA" ->
                let x = comms.recv_int S () in
                assume (((x) >= (0)));
                callbacks.state11OnreceivequoteA st x;
                let st : state12 = {
                    _dumstate12 = ();
                    id = (Mkstate11?.id st);
                    x = x;
                }
                in
                runState12 st
            | _ -> unexpected "unexpected label"
    and runState12 (st: state12) : ML unit =
        comms.send_string B "proposeA";
        let a = callbacks.state12OnsendproposeA st in
        comms.send_int B a;
        let st : state13 = {
            _dumstate13 = ();
            id = (Mkstate12?.id st);
            x = (Mkstate12?.x st);
            a = a;
        }
        in
        runState13 st
    and runState13 (st: state13) : ML unit =
        let label = comms.recv_string B () in
        match label with
            | "ok" ->
                let b = comms.recv_int B () in
                callbacks.state13Onreceiveok st b;
                let st : state15 = {
                    _dumstate15 = ();
                    id = (Mkstate13?.id st);
                    x = (Mkstate13?.x st);
                    a = (Mkstate13?.a st);
                    b = b;
                }
                in
                runState15 st
            | "no" ->
                let _dummy = comms.recv_unit B () in
                callbacks.state13Onreceiveno st ;
                let st : state14 = {
                    _dumstate14 = ();
                    id = (Mkstate13?.id st);
                    x = (Mkstate13?.x st);
                    a = (Mkstate13?.a st);
                }
                in
                runState14 st
            | _ -> unexpected "unexpected label"
    and runState14 (st: state14) : ML unit =
        comms.send_string S "cancel";
        let _dummy = callbacks.state14Onsendcancel st in
        comms.send_unit S _dummy;
        let st : state10 = ()
        in
        runState10 st
    and runState15 (st: state15) : ML unit =
        comms.send_string S "buy";
        let _dummy = callbacks.state15Onsendbuy st in
        comms.send_unit S _dummy;
        let st : state10 = ()
        in
        runState10 st
    in
    runState9 ()
