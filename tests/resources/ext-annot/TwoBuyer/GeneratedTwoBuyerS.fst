module GeneratedTwoBuyerS
(* This file is GENERATED, do not modify manually *)
open FStar.All
open FStar.Error

type role =
    | A
    | B
type state10 = {
    _dumstate10 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
}
and state11 = {
    _dumstate11 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
    y : (y : int{((x) = (y))});
}
and state7 = unit
and state8 = unit
and state9 = {
    _dumstate9 : unit;
    id : int;
}
noeq type callbacksS = {
    state7OnreceivebookId : (st: state7) -> (id: int) -> ML (unit);
    state9OnsendquoteA : (st: state9) -> ML (x:int{((x) >= (0))});
    state10OnsendquoteB : (st: state10) -> ML (y:int{((Mkstate10?.x st) = (y))});
    state11Onreceivebuy : (st: state11) -> ML (unit);
    state11Onreceivecancel : (st: state11) -> ML (unit);
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
    let rec runState7 (st: state7) : ML unit =
        let label = comms.recv_string A () in
        match label with
            | "bookId" ->
                let id = comms.recv_int A () in
                callbacks.state7OnreceivebookId st id;
                let st : state9 = {
                    _dumstate9 = ();
                    id = id;
                }
                in
                runState9 st
            | _ -> unexpected "unexpected label"
    and runState8 (st: state8) : ML unit =
        ()
    and runState9 (st: state9) : ML unit =
        comms.send_string A "quoteA";
        let x = callbacks.state9OnsendquoteA st in
        comms.send_int A x;
        let st : state10 = {
            _dumstate10 = ();
            id = (Mkstate9?.id st);
            x = x;
        }
        in
        runState10 st
    and runState10 (st: state10) : ML unit =
        comms.send_string B "quoteB";
        let y = callbacks.state10OnsendquoteB st in
        comms.send_int B y;
        let st : state11 = {
            _dumstate11 = ();
            id = (Mkstate10?.id st);
            x = (Mkstate10?.x st);
            y = y;
        }
        in
        runState11 st
    and runState11 (st: state11) : ML unit =
        let label = comms.recv_string A () in
        match label with
            | "buy" ->
                let _dum1 = comms.recv_unit A () in
                callbacks.state11Onreceivebuy st ;
                let st : state8 = ()
                in
                runState8 st
            | "cancel" ->
                let _dum3 = comms.recv_unit A () in
                callbacks.state11Onreceivecancel st ;
                let st : state8 = ()
                in
                runState8 st
            | _ -> unexpected "unexpected label"
    in
    runState7 ()
