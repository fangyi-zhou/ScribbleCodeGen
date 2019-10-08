module ScribbleGeneratedAdderC
(* This file is GENERATED, do not modify manually *)
open FluidTypes.Annotations

type Role =
    | S
type State10Choice =
    | BYE = 0
    | ADD = 1
and State10_ADD = {
    u : int
}
and State10_BYE = {
    u : int
}
type State10 = {
    u : int
}
and State11 = {
    u : int
    x : int
}
and State12 = {
    u : int
    x : int
    y : int
}
and State13 = {
    u : int
}
and State8 = unit
and State9 = unit
type CallbacksC = {
    [<Refined("(st: State8) -> int")>] state8OnsendHELLO : State8 -> int
    [<Refined("(st: State10) -> {choice:int|(((choice) = (Choice10BYE)) || ((choice) = (Choice10ADD)))}")>] state10 : State10 -> State10Choice
    [<Refined("(st: State10_BYE) -> unit")>] state10OnsendBYE : State10_BYE -> unit
    [<Refined("(st: State10_ADD) -> int")>] state10OnsendADD : State10_ADD -> int
    [<Refined("(st: State11) -> int")>] state11OnsendADD : State11 -> int
    [<Refined("(st: State12) -> (z: {z:int|((z) = (((st)$x) + ((st)$y)))}) -> unit")>] state12OnreceiveRES : State12 -> int -> unit
    [<Refined("(st: State13) -> unit")>] state13OnreceiveBYE : State13 -> unit
}
type Communications = {
    send_int : Role -> int -> Async<unit>;
    send_string : Role -> string -> Async<unit>;
    send_unit : Role -> unit -> Async<unit>;
    recv_int : Role -> unit -> Async<int>;
    recv_string : Role -> unit -> Async<string>;
    recv_unit : Role -> unit -> Async<unit>;
}
let run (callbacks : CallbacksC) (comms : Communications) : Async<unit> =
    let rec runState8 (st: State8) : Async<unit> =
        async {
            do! comms.send_string S "HELLO"
            let u = callbacks.state8OnsendHELLO st
            do! comms.send_int S u
            let st : State10 = {
                u = u;
            }
            return! runState10 st
        }
    and runState9 (st: State9) : Async<unit> =
        async {
            ()
        }
    and runState10 (st: State10) : Async<unit> =
        async {
            let label = callbacks.state10 st
            match label with
                | State10Choice.BYE ->
                    let st : State10_BYE = {
                        u = st.u;
                    }
                    do! comms.send_string S "BYE"
                    let _dum1 = callbacks.state10OnsendBYE st
                    do! comms.send_unit S _dum1
                    let st : State13 = {
                        u = st.u;
                    }
                    return! runState13 st
                | State10Choice.ADD ->
                    let st : State10_ADD = {
                        u = st.u;
                    }
                    do! comms.send_string S "ADD"
                    let x = callbacks.state10OnsendADD st
                    do! comms.send_int S x
                    let st : State11 = {
                        u = st.u;
                        x = x;
                    }
                    return! runState11 st
        }
    and runState11 (st: State11) : Async<unit> =
        async {
            do! comms.send_string S "ADD"
            let y = callbacks.state11OnsendADD st
            do! comms.send_int S y
            let st : State12 = {
                u = st.u;
                x = st.x;
                y = y;
            }
            return! runState12 st
        }
    and runState12 (st: State12) : Async<unit> =
        async {
            let! label = comms.recv_string S ()
            match label with
                | "RES" ->
                    let! z = comms.recv_int S ()
                    callbacks.state12OnreceiveRES st z
                    let st : State8 = ()
                    return! runState8 st
                | _ -> failwith "unexpected label"
        }
    and runState13 (st: State13) : Async<unit> =
        async {
            let! label = comms.recv_string S ()
            match label with
                | "BYE" ->
                    let! _dum2 = comms.recv_unit S ()
                    callbacks.state13OnreceiveBYE st 
                    let st : State9 = ()
                    return! runState9 st
                | _ -> failwith "unexpected label"
        }
    let initState : State8 =
        ()
    runState8 initState
