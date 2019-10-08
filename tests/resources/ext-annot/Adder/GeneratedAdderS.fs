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
    x : int
}
and State24 = {
    u : int
    x : int
    y : int
}
and State25 = {
    u : int
}
type CallbacksS = {
    [<Refined("(st: State20) -> (u: int) -> unit")>] state20OnreceiveHELLO : State20 -> int -> unit
    [<Refined("(st: State22) -> unit")>] state22OnreceiveBYE : State22 -> unit
    [<Refined("(st: State22) -> (x: int) -> unit")>] state22OnreceiveADD : State22 -> int -> unit
    [<Refined("(st: State23) -> (y: int) -> unit")>] state23OnreceiveADD : State23 -> int -> unit
    [<Refined("(st: State24) -> {z:int|((z) = (((st)$x) + ((st)$y)))}")>] state24OnsendRES : State24 -> int
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
                    let! _dum1 = comms.recv_unit C ()
                    callbacks.state22OnreceiveBYE st 
                    let st : State25 = {
                        u = st.u;
                    }
                    return! runState25 st
                | "ADD" ->
                    let! x = comms.recv_int C ()
                    callbacks.state22OnreceiveADD st x
                    let st : State23 = {
                        u = st.u;
                        x = x;
                    }
                    return! runState23 st
                | _ -> failwith "unexpected label"
        }
    and runState23 (st: State23) : Async<unit> =
        async {
            let! label = comms.recv_string C ()
            match label with
                | "ADD" ->
                    let! y = comms.recv_int C ()
                    callbacks.state23OnreceiveADD st y
                    let st : State24 = {
                        u = st.u;
                        x = st.x;
                        y = y;
                    }
                    return! runState24 st
                | _ -> failwith "unexpected label"
        }
    and runState24 (st: State24) : Async<unit> =
        async {
            do! comms.send_string C "RES"
            let z = callbacks.state24OnsendRES st
            do! comms.send_int C z
            let st : State20 = ()
            return! runState20 st
        }
    and runState25 (st: State25) : Async<unit> =
        async {
            do! comms.send_string C "BYE"
            let _dum2 = callbacks.state25OnsendBYE st
            do! comms.send_unit C _dum2
            let st : State21 = ()
            return! runState21 st
        }
    let initState : State20 =
        ()
    runState20 initState
