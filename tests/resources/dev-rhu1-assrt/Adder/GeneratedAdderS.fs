module ScribbleGeneratedAdderS
(* This file is GENERATED, do not modify manually *)
open FluidTypes.Annotations

type Role =
    | C
type State20 = unit
and State21 = unit
and State22 = {
    u : int
}
and State23 = {
    u : int
    w : int
}
and State24 = {
    u : int
    w : int
    v : int
}
and State25 = {
    u : int
}
type CallbacksS = {
    [<Refined("(st: State20) -> (u: int) -> unit")>] state20OnreceiveHELLO : State20 -> int -> unit
    [<Refined("(st: State22) -> unit")>] state22OnreceiveBYE : State22 -> unit
    [<Refined("(st: State22) -> (w: int) -> unit")>] state22OnreceiveADD : State22 -> int -> unit
    [<Refined("(st: State23) -> (v: int) -> unit")>] state23OnreceiveADD : State23 -> int -> unit
    [<Refined("(st: State24) -> int")>] state24OnsendRES : State24 -> int
    [<Refined("(st: State25) -> unit")>] state25OnsendBYE : State25 -> unit
}
type Communications = {
    send_int : Role -> int -> Async<unit>;
    send_string : Role -> string -> Async<unit>;
    send_unit : Role -> unit -> Async<unit>;
    recv_int : Role -> unit -> Async<int>;
    recv_string : Role -> unit -> Async<string>;
    recv_unit : Role -> unit -> Async<unit>;
}
let run (callbacks : CallbacksS) (comms : Communications) : Async<unit> =
    let rec runState20 (st: State20) : Async<unit> =
        async {
            let! label = comms.recv_string C ()
            match label with
                | "HELLO" ->
                    let! u = comms.recv_int C ()
                    callbacks.state20OnreceiveHELLO st u
                    let st : State22 = {
                        u = u;
                    }
                    return! runState22 st
                | _ -> failwith "unexpected label"
        }
    and runState21 (st: State21) : Async<unit> =
        async {
            ()
        }
    and runState22 (st: State22) : Async<unit> =
        async {
            let! label = comms.recv_string C ()
            match label with
                | "BYE" ->
                    let! _dummy = comms.recv_unit C ()
                    callbacks.state22OnreceiveBYE st 
                    let st : State25 = {
                        u = st.u;
                    }
                    return! runState25 st
                | "ADD" ->
                    let! w = comms.recv_int C ()
                    callbacks.state22OnreceiveADD st w
                    let st : State23 = {
                        u = st.u;
                        w = w;
                    }
                    return! runState23 st
                | _ -> failwith "unexpected label"
        }
    and runState23 (st: State23) : Async<unit> =
        async {
            let! label = comms.recv_string C ()
            match label with
                | "ADD" ->
                    let! v = comms.recv_int C ()
                    callbacks.state23OnreceiveADD st v
                    let st : State24 = {
                        u = st.u;
                        w = st.w;
                        v = v;
                    }
                    return! runState24 st
                | _ -> failwith "unexpected label"
        }
    and runState24 (st: State24) : Async<unit> =
        async {
            do! comms.send_string C "RES"
            let f = callbacks.state24OnsendRES st
            do! comms.send_int C f
            let st : State20 = ()
            return! runState20 st
        }
    and runState25 (st: State25) : Async<unit> =
        async {
            do! comms.send_string C "BYE"
            let _dummy = callbacks.state25OnsendBYE st
            do! comms.send_unit C _dummy
            let st : State21 = ()
            return! runState21 st
        }
    let initState : State20 =
        ()
    runState20 initState
