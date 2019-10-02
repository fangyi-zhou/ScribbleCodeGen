module ScribbleGeneratedTwoBuyerA
(* This file is GENERATED, do not modify manually *)
open FluidTypes.Annotations

type Role =
    | B
    | S
type State19 = unit
and State20 = unit
and State21 = {
    id : int
}
and State22 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
}
and State23 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{a:int|((a) >= (0)) && ((a) <= (x))}")>] a : int
}
and State24 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{a:int|((a) >= (0)) && ((a) <= (x))}")>] a : int
}
and State25 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{a:int|((a) >= (0)) && ((a) <= (x))}")>] a : int
    [<Refined("{b:int|((b) <= (a))}")>] b : int
}
type CallbacksA = {
    [<Refined("(st: State19) -> int")>] state19OnsendbookId : State19 -> int
    [<Refined("(st: State21) -> (x: {x:int|((x) >= (0))}) -> unit")>] state21OnreceivequoteA : State21 -> int -> unit
    [<Refined("(st: State22) -> {a:int|((a) >= (0)) && ((a) <= ((st)$x))}")>] state22OnsendproposeA : State22 -> int
    [<Refined("(st: State23) -> (b: {b:int|((b) <= ((st)$a))}) -> unit")>] state23Onreceiveok : State23 -> int -> unit
    [<Refined("(st: State23) -> unit")>] state23Onreceiveno : State23 -> unit
    [<Refined("(st: State24) -> unit")>] state24Onsendcancel : State24 -> unit
    [<Refined("(st: State25) -> unit")>] state25Onsendbuy : State25 -> unit
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
    let rec runState19 (st: State19) : Async<unit> =
        async {
            do! comms.send_string S "bookId"
            let id = callbacks.state19OnsendbookId st
            do! comms.send_int S id
            let st : State21 = {
                id = id;
            }
            return! runState21 st
        }
    and runState20 (st: State20) : Async<unit> =
        async {
            ()
        }
    and runState21 (st: State21) : Async<unit> =
        async {
            let! label = comms.recv_string S ()
            match label with
                | "quoteA" ->
                    let! x = comms.recv_int S ()
                    callbacks.state21OnreceivequoteA st x
                    let st : State22 = {
                        id = st.id;
                        x = x;
                    }
                    return! runState22 st
                | _ -> failwith "unexpected label"
        }
    and runState22 (st: State22) : Async<unit> =
        async {
            do! comms.send_string B "proposeA"
            let a = callbacks.state22OnsendproposeA st
            do! comms.send_int B a
            let st : State23 = {
                id = st.id;
                x = st.x;
                a = a;
            }
            return! runState23 st
        }
    and runState23 (st: State23) : Async<unit> =
        async {
            let! label = comms.recv_string B ()
            match label with
                | "ok" ->
                    let! b = comms.recv_int B ()
                    callbacks.state23Onreceiveok st b
                    let st : State25 = {
                        id = st.id;
                        x = st.x;
                        a = st.a;
                        b = b;
                    }
                    return! runState25 st
                | "no" ->
                    let! _dum2 = comms.recv_unit B ()
                    callbacks.state23Onreceiveno st 
                    let st : State24 = {
                        id = st.id;
                        x = st.x;
                        a = st.a;
                    }
                    return! runState24 st
                | _ -> failwith "unexpected label"
        }
    and runState24 (st: State24) : Async<unit> =
        async {
            do! comms.send_string S "cancel"
            let _dum3 = callbacks.state24Onsendcancel st
            do! comms.send_unit S _dum3
            let st : State20 = ()
            return! runState20 st
        }
    and runState25 (st: State25) : Async<unit> =
        async {
            do! comms.send_string S "buy"
            let _dum1 = callbacks.state25Onsendbuy st
            do! comms.send_unit S _dum1
            let st : State20 = ()
            return! runState20 st
        }
    runState19 ()
