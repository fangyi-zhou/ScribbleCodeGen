module GeneratedFibB
(* This file is GENERATED, do not modify manually *)
open FStar.All
open FStar.Error

type role =
    | A
type state17 = {
    _dumstate17 : unit;
    x : (x : int{((x) >= (0))});
    y : (y : int{((y) >= (x))});
}
and state19 = {
    _dumstate19 : unit;
    x : (x : int{((x) >= (0))});
    y : (y : int{((y) >= (x))});
    x1 : (x1 : int{((x1) = (x))});
}
and state20 = {
    _dumstate20 : unit;
    x : (x : int{((x) >= (0))});
    y : (y : int{((y) >= (x))});
    x1 : (x1 : int{((x1) = (x))});
    y1 : (y1 : int{((y1) = (y))});
}
noeq type callbacksB = {
    state17Onreceive1 : (st: state17) -> (x1: int{((x1) = (Mkstate17?.x st))}) -> ML (unit);
    state19Onreceive2 : (st: state19) -> (y1: int{((y1) = (Mkstate19?.y st))}) -> ML (unit);
    state20Onsend3 : (st: state20) -> ML (z1:int{((z1) = ((Mkstate20?.x1 st) + (Mkstate20?.y1 st)))});
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
    let rec runState17 (st: state17) : ML unit =
        let label = comms.recv_string A () in
        match label with
            | "1" ->
                let x1 = comms.recv_int A () in
                assume (((x1) = (Mkstate17?.x st)));
                callbacks.state17Onreceive1 st x1;
                let st : state19 = {
                    _dumstate19 = ();
                    x = (Mkstate17?.x st);
                    y = (Mkstate17?.y st);
                    x1 = x1;
                }
                in
                runState19 st
            | _ -> unexpected "unexpected label"
    and runState19 (st: state19) : ML unit =
        let label = comms.recv_string A () in
        match label with
            | "2" ->
                let y1 = comms.recv_int A () in
                assume (((y1) = (Mkstate19?.y st)));
                callbacks.state19Onreceive2 st y1;
                let st : state20 = {
                    _dumstate20 = ();
                    x = (Mkstate19?.x st);
                    y = (Mkstate19?.y st);
                    x1 = (Mkstate19?.x1 st);
                    y1 = y1;
                }
                in
                runState20 st
            | _ -> unexpected "unexpected label"
    and runState20 (st: state20) : ML unit =
        comms.send_string A "3";
        let z1 = callbacks.state20Onsend3 st in
        comms.send_int A z1;
        let st : state17 = {
            _dumstate17 = ();
            x = (Mkstate20?.y st);
            y = (z1);
        }
        in
        runState17 st
    in
    let initState : state17 =
        {
            _dumstate17 = ();
            x = 0;
            y = 1;
        }
    in
    runState17 initState
