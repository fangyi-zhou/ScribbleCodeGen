module GeneratedTwoBuyerS
(* This file is GENERATED, do not modify manually *)
open FStar.All
open FStar.Error

type role =
    | A
    | B
type state35 = unit
and state36 = unit
and state37 = {
    _dumstate37 : unit;
    id : int;
}
and state38 = {
    _dumstate38 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
}
and state39 = {
    _dumstate39 : unit;
    id : int;
    x : (x : int{((x) >= (0))});
    y : (y : int{((x) = (y))});
}
noeq type callbacksS = {
    state35OnreceivebookId : (st: state35) -> (id: int) -> ML (unit);
    state37OnsendquoteA : (st: state37) -> ML (x:int{((x) >= (0))});
    state38OnsendquoteB : (st: state38) -> ML (y:int{((Mkstate38?.x st) = (y))});
    state39Onreceivebuy : (st: state39) -> ML (unit);
    state39Onreceivecancel : (st: state39) -> ML (unit);
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
    let rec runState35 (st: state35) : ML unit =
        let label = comms.recv_string A () in
        match label with
            | "bookId" ->
                let id = comms.recv_int A () in
                callbacks.state35OnreceivebookId st id;
                let st : state37 = {
                    _dumstate37 = ();
                    id = id;
                }
                in
                runState37 st
            | _ -> unexpected "unexpected label"
    and runState36 (st: state36) : ML unit =
        ()
    and runState37 (st: state37) : ML unit =
        comms.send_string A "quoteA";
        let x = callbacks.state37OnsendquoteA st in
        comms.send_int A x;
        let st : state38 = {
            _dumstate38 = ();
            id = (Mkstate37?.id st);
            x = x;
        }
        in
        runState38 st
    and runState38 (st: state38) : ML unit =
        comms.send_string B "quoteB";
        let y = callbacks.state38OnsendquoteB st in
        comms.send_int B y;
        let st : state39 = {
            _dumstate39 = ();
            id = (Mkstate38?.id st);
            x = (Mkstate38?.x st);
            y = y;
        }
        in
        runState39 st
    and runState39 (st: state39) : ML unit =
        let label = comms.recv_string A () in
        match label with
            | "buy" ->
                let _ = comms.recv_unit A () in
                callbacks.state39Onreceivebuy st ;
                let st : state36 = ()
                in
                runState36 st
            | "cancel" ->
                let _ = comms.recv_unit A () in
                callbacks.state39Onreceivecancel st ;
                let st : state36 = ()
                in
                runState36 st
            | _ -> unexpected "unexpected label"
    in
    runState35 ()
