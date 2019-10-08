module ScribbleGeneratedFibA
(* This file is GENERATED, do not modify manually *)
open FluidTypes.Annotations

type Role =
    | B
type State6 = {
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{y:int|((y) >= (x))}")>] y : int
}
and State8 = {
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{y:int|((y) >= (x))}")>] y : int
    [<Refined("{x1:int|((x1) = (x))}")>] x1 : int
}
and State9 = {
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{y:int|((y) >= (x))}")>] y : int
    [<Refined("{x1:int|((x1) = (x))}")>] x1 : int
    [<Refined("{y1:int|((y1) = (y))}")>] y1 : int
}
type CallbacksA = {
    [<Refined("(st: State6) -> {x1:int|((x1) = ((st)$x))}")>] state6Onsend1 : State6 -> int
    [<Refined("(st: State8) -> {y1:int|((y1) = ((st)$y))}")>] state8Onsend2 : State8 -> int
    [<Refined("(st: State9) -> (z1: {z1:int|((z1) = (((st)$x1) + ((st)$y1)))}) -> unit")>] state9Onreceive3 : State9 -> int -> unit
}
type Communications = {
    send_int : Role -> int -> Async<unit>;
    send_string : Role -> string -> Async<unit>;
    send_unit : Role -> unit -> Async<unit>;
    recv_int : Role -> unit -> Async<int>;
    recv_string : Role -> unit -> Async<string>;
    recv_unit : Role -> unit -> Async<unit>;
}
let run (callbacks : CallbacksA) (comms : Communications) : Async<unit> =
    let rec runState6 (st: State6) : Async<unit> =
        async {
            do! comms.send_string B "1"
            let x1 = callbacks.state6Onsend1 st
            do! comms.send_int B x1
            let st : State8 = {
                x = st.x;
                y = st.y;
                x1 = x1;
            }
            return! runState8 st
        }
    and runState8 (st: State8) : Async<unit> =
        async {
            do! comms.send_string B "2"
            let y1 = callbacks.state8Onsend2 st
            do! comms.send_int B y1
            let st : State9 = {
                x = st.x;
                y = st.y;
                x1 = st.x1;
                y1 = y1;
            }
            return! runState9 st
        }
    and runState9 (st: State9) : Async<unit> =
        async {
            let! label = comms.recv_string B ()
            match label with
                | "3" ->
                    let! z1 = comms.recv_int B ()
                    callbacks.state9Onreceive3 st z1
                    let st : State6 = {
                        x = (st.y);
                        y = (z1);
                    }
                    return! runState6 st
                | _ -> failwith "unexpected label"
        }
    let initState : State6 =
        {
            x = 0;
            y = 1;
        }
    runState6 initState
