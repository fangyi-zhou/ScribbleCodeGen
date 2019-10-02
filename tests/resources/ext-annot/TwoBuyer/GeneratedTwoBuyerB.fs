module ScribbleGeneratedTwoBuyerB
(* This file is GENERATED, do not modify manually *)
open FluidTypes.Annotations

type Role =
    | A
    | S
type State33Choice =
    | ok = 0
    | no = 1
and State33_no = {
    y : int
    [<Refined("{a:int|((a) >= (0))}")>] a : int
}
and State33_ok = {
    y : int
    [<Refined("{a:int|((a) >= (0)) && (((y) - (a)) <= (a))}")>] a : int
}
type State30 = unit
and State31 = unit
and State32 = {
    y : int
}
and State33 = {
    y : int
    [<Refined("{a:int|((a) >= (0))}")>] a : int
}
type CallbacksB = {
    [<Refined("(st: State30) -> (y: int) -> unit")>] state30OnreceivequoteB : State30 -> int -> unit
    [<Refined("(st: State32) -> (a: {a:int|((a) >= (0))}) -> unit")>] state32OnreceiveproposeA : State32 -> int -> unit
    [<Refined("(st: State33) -> {choice:int|((((choice) = (Choice33ok)) && ((((st)$y) - ((st)$a)) <= ((st)$a))) || ((choice) = (Choice33no)))}")>] state33 : State33 -> State33Choice
    [<Refined("(st: State33_ok) -> {b:int|((b) = (((st)$y) - ((st)$a))) && ((((st)$y) - ((st)$a)) <= ((st)$a))}")>] state33Onsendok : State33_ok -> int
    [<Refined("(st: State33_no) -> unit")>] state33Onsendno : State33_no -> unit
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
    let rec runState30 (st: State30) : Async<unit> =
        async {
            let! label = comms.recv_string S ()
            match label with
                | "quoteB" ->
                    let! y = comms.recv_int S ()
                    callbacks.state30OnreceivequoteB st y
                    let st : State32 = {
                        y = y;
                    }
                    return! runState32 st
                | _ -> failwith "unexpected label"
        }
    and runState31 (st: State31) : Async<unit> =
        async {
            ()
        }
    and runState32 (st: State32) : Async<unit> =
        async {
            let! label = comms.recv_string A ()
            match label with
                | "proposeA" ->
                    let! a = comms.recv_int A ()
                    callbacks.state32OnreceiveproposeA st a
                    let st : State33 = {
                        y = st.y;
                        a = a;
                    }
                    return! runState33 st
                | _ -> failwith "unexpected label"
        }
    and runState33 (st: State33) : Async<unit> =
        async {
            let label = callbacks.state33 st
            match label with
                | State33Choice.ok ->
                    let st : State33_ok = {
                        y = st.y;
                        a = st.a;
                    }
                    do! comms.send_string A "ok"
                    let b = callbacks.state33Onsendok st
                    do! comms.send_int A b
                    let st : State31 = ()
                    return! runState31 st
                | State33Choice.no ->
                    let st : State33_no = {
                        y = st.y;
                        a = st.a;
                    }
                    do! comms.send_string A "no"
                    let _dum2 = callbacks.state33Onsendno st
                    do! comms.send_unit A _dum2
                    let st : State31 = ()
                    return! runState31 st
        }
    runState30 ()
