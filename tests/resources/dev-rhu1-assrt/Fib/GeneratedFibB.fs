module ScribbleGeneratedFibB
(* This file is GENERATED, do not modify manually *)
open FluidTypes.Annotations

type Role =
    | A
type State17 = {
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{y:int|((y) >= (x))}")>] y : int
}
and State19 = {
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{y:int|((y) >= (x))}")>] y : int
    [<Refined("{x1:int|((x1) = (x))}")>] x1 : int
}
and State20 = {
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{y:int|((y) >= (x))}")>] y : int
    [<Refined("{x1:int|((x1) = (x))}")>] x1 : int
    [<Refined("{y1:int|((y1) = (y))}")>] y1 : int
}
type CallbacksB = {
    [<Refined("(st: State17) -> (x1: {x1:int|((x1) = ((st)$x))}) -> unit")>] state17Onreceive1 : State17 -> int -> unit
    [<Refined("(st: State19) -> (y1: {y1:int|((y1) = ((st)$y))}) -> unit")>] state19Onreceive2 : State19 -> int -> unit
    [<Refined("(st: State20) -> {z1:int|((z1) = (((st)$x1) + ((st)$y1)))}")>] state20Onsend3 : State20 -> int
}
type Communications = {
    send_int : Role -> int -> Async<unit>;
    send_string : Role -> string -> Async<unit>;
    send_unit : Role -> unit -> Async<unit>;
    recv_int : Role -> unit -> Async<int>;
    recv_string : Role -> unit -> Async<string>;
    recv_unit : Role -> unit -> Async<unit>;
}
let run (callbacks : CallbacksB) (comms : Communications) : Async<unit> =
    let rec runState17 (st: State17) : Async<unit> =
        async {
            let! label = comms.recv_string A ()
            match label with
                | "1" ->
                    let! x1 = comms.recv_int A ()
                    callbacks.state17Onreceive1 st x1
                    let st : State19 = {
                        x = st.x;
                        y = st.y;
                        x1 = x1;
                    }
                    return! runState19 st
                | _ -> failwith "unexpected label"
        }
    and runState19 (st: State19) : Async<unit> =
        async {
            let! label = comms.recv_string A ()
            match label with
                | "2" ->
                    let! y1 = comms.recv_int A ()
                    callbacks.state19Onreceive2 st y1
                    let st : State20 = {
                        x = st.x;
                        y = st.y;
                        x1 = st.x1;
                        y1 = y1;
                    }
                    return! runState20 st
                | _ -> failwith "unexpected label"
        }
    and runState20 (st: State20) : Async<unit> =
        async {
            do! comms.send_string A "3"
            let z1 = callbacks.state20Onsend3 st
            do! comms.send_int A z1
            let st : State17 = {
                x = (st.y);
                y = (z1);
            }
            return! runState17 st
        }
    let initState : State17 =
        {
            x = 0;
            y = 1;
        }
    runState17 initState
