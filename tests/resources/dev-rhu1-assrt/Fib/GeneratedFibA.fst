module GeneratedFibA
(* This file is GENERATED, do not modify manually *)
open FStar.All
open FStar.Error

type role =
    | B
type state6 = {
    _dumstate6 : unit;
    x : (x : int{((x) >= (0))});
    y : (y : int{((y) >= (x))});
}
and state8 = {
    _dumstate8 : unit;
    x : (x : int{((x) >= (0))});
    y : (y : int{((y) >= (x))});
    x1 : (x1 : int{((x1) = (x))});
}
and state9 = {
    _dumstate9 : unit;
    x : (x : int{((x) >= (0))});
    y : (y : int{((y) >= (x))});
    x1 : (x1 : int{((x1) = (x))});
    y1 : (y1 : int{((y1) = (y))});
}
noeq type callbacksA = {
    state6Onsend1 : (st: state6) -> ML (x1:int{((x1) = (Mkstate6?.x st))});
    state8Onsend2 : (st: state8) -> ML (y1:int{((y1) = (Mkstate8?.y st))});
    state9Onreceive3 : (st: state9) -> (z1: int{((z1) = ((Mkstate9?.x1 st) + (Mkstate9?.y1 st)))}) -> ML (unit);
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
    let rec runState6 (st: state6) : ML unit =
        comms.send_string B "1";
        let x1 = callbacks.state6Onsend1 st in
        comms.send_int B x1;
        let st : state8 = {
            _dumstate8 = ();
            x = (Mkstate6?.x st);
            y = (Mkstate6?.y st);
            x1 = x1;
        }
        in
        runState8 st
    and runState8 (st: state8) : ML unit =
        comms.send_string B "2";
        let y1 = callbacks.state8Onsend2 st in
        comms.send_int B y1;
        let st : state9 = {
            _dumstate9 = ();
            x = (Mkstate8?.x st);
            y = (Mkstate8?.y st);
            x1 = (Mkstate8?.x1 st);
            y1 = y1;
        }
        in
        runState9 st
    and runState9 (st: state9) : ML unit =
        let label = comms.recv_string B () in
        match label with
            | "3" ->
                let z1 = comms.recv_int B () in
                assume (((z1) = ((Mkstate9?.x1 st) + (Mkstate9?.y1 st))));
                callbacks.state9Onreceive3 st z1;
                let st : state6 = {
                    _dumstate6 = ();
                    x = (Mkstate9?.y st);
                    y = (z1);
                }
                in
                runState6 st
            | _ -> unexpected "unexpected label"
    in
    let initState : state6 =
        {
            _dumstate6 = ();
            x = 0;
            y = 1;
        }
    in
    runState6 initState
