module ScribbleGeneratedTwoBuyerB
(* This file is GENERATED, do not modify manually *)
open FluidTypes.Annotations

type Role =
    | A
    | S
type State26Choice =
    | no = 0
    | ok = 1
and State26_no = {
    y : int
    [<Refined("{a:int|((a) >= (0))}")>] a : int
}
and State26_ok = {
    y : int
    [<Refined("{a:int|((a) >= (0)) && (((y) - (a)) <= (a))}")>] a : int
}
type State23 = unit
and State24 = unit
and State25 = {
    y : int
}
and State26 = {
    y : int
    [<Refined("{a:int|((a) >= (0))}")>] a : int
}
type CallbacksB = {
    [<Refined("(st: State23) -> (y: int) -> unit")>] state23OnreceivequoteB : State23 -> int -> unit
    [<Refined("(st: State25) -> (a: {a:int|((a) >= (0))}) -> unit")>] state25OnreceiveproposeA : State25 -> int -> unit
    [<Refined("(st: State26) -> {choice:int|(((choice) = (Choice26no)) || (((choice) = (Choice26ok)) && ((((st)$y) - ((st)$a)) <= ((st)$a))))}")>] state26 : State26 -> State26Choice
    [<Refined("(st: State26_no) -> unit")>] state26Onsendno : State26_no -> unit
    [<Refined("(st: State26_ok) -> {b:int|((b) = (((st)$y) - ((st)$a))) && ((((st)$y) - ((st)$a)) <= ((st)$a))}")>] state26Onsendok : State26_ok -> int
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
    let rec runState23 (st: State23) : Async<unit> =
        async {
            let! label = comms.recv_string S ()
            match label with
                | "quoteB" ->
                    let! y = comms.recv_int S ()
                    callbacks.state23OnreceivequoteB st y
                    let st : State25 = {
                        y = y;
                    }
                    return! runState25 st
                | _ -> failwith "unexpected label"
        }
    and runState24 (st: State24) : Async<unit> =
        async {
            ()
        }
    and runState25 (st: State25) : Async<unit> =
        async {
            let! label = comms.recv_string A ()
            match label with
                | "proposeA" ->
                    let! a = comms.recv_int A ()
                    callbacks.state25OnreceiveproposeA st a
                    let st : State26 = {
                        y = st.y;
                        a = a;
                    }
                    return! runState26 st
                | _ -> failwith "unexpected label"
        }
    and runState26 (st: State26) : Async<unit> =
        async {
            let label = callbacks.state26 st
            match label with
                | State26Choice.no ->
                    let st : State26_no = {
                        y = st.y;
                        a = st.a;
                    }
                    do! comms.send_string A "no"
                    let _ = callbacks.state26Onsendno st
                    do! comms.send_unit A _
                    let st : State24 = ()
                    return! runState24 st
                | State26Choice.ok ->
                    let st : State26_ok = {
                        y = st.y;
                        a = st.a;
                    }
                    do! comms.send_string A "ok"
                    let b = callbacks.state26Onsendok st
                    do! comms.send_int A b
                    let st : State24 = ()
                    return! runState24 st
        }
    runState23 ()
