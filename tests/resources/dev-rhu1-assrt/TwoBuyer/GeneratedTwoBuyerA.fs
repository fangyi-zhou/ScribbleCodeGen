module ScribbleGeneratedTwoBuyerA
(* This file is GENERATED, do not modify manually *)
open FluidTypes.Annotations

type Role =
    | B
    | S
type State10 = unit
and State11 = {
    id : int
}
and State12 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
}
and State13 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{a:int|((a) >= (0)) && ((a) <= (x))}")>] a : int
}
and State14 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{a:int|((a) >= (0)) && ((a) <= (x))}")>] a : int
}
and State15 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{a:int|((a) >= (0)) && ((a) <= (x))}")>] a : int
    b : int
}
and State9 = unit
type CallbacksA = {
    [<Refined("(st: State9) -> int")>] state9OnsendbookId : State9 -> int
    [<Refined("(st: State11) -> (x: {x:int|((x) >= (0))}) -> unit")>] state11OnreceivequoteA : State11 -> int -> unit
    [<Refined("(st: State12) -> {a:int|((a) >= (0)) && ((a) <= ((st)$x))}")>] state12OnsendproposeA : State12 -> int
    [<Refined("(st: State13) -> (b: int) -> unit")>] state13Onreceiveok : State13 -> int -> unit
    [<Refined("(st: State13) -> unit")>] state13Onreceiveno : State13 -> unit
    [<Refined("(st: State14) -> unit")>] state14Onsendcancel : State14 -> unit
    [<Refined("(st: State15) -> unit")>] state15Onsendbuy : State15 -> unit
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
    let rec runState9 (st: State9) : Async<unit> =
        async {
            do! comms.send_string S "bookId"
            let id = callbacks.state9OnsendbookId st
            do! comms.send_int S id
            let st : State11 = {
                id = id;
            }
            return! runState11 st
        }
    and runState10 (st: State10) : Async<unit> =
        async {
            ()
        }
    and runState11 (st: State11) : Async<unit> =
        async {
            let! label = comms.recv_string S ()
            match label with
                | "quoteA" ->
                    let! x = comms.recv_int S ()
                    callbacks.state11OnreceivequoteA st x
                    let st : State12 = {
                        id = st.id;
                        x = x;
                    }
                    return! runState12 st
                | _ -> failwith "unexpected label"
        }
    and runState12 (st: State12) : Async<unit> =
        async {
            do! comms.send_string B "proposeA"
            let a = callbacks.state12OnsendproposeA st
            do! comms.send_int B a
            let st : State13 = {
                id = st.id;
                x = st.x;
                a = a;
            }
            return! runState13 st
        }
    and runState13 (st: State13) : Async<unit> =
        async {
            let! label = comms.recv_string B ()
            match label with
                | "ok" ->
                    let! b = comms.recv_int B ()
                    callbacks.state13Onreceiveok st b
                    let st : State15 = {
                        id = st.id;
                        x = st.x;
                        a = st.a;
                        b = b;
                    }
                    return! runState15 st
                | "no" ->
                    let! _ = comms.recv_unit B ()
                    callbacks.state13Onreceiveno st 
                    let st : State14 = {
                        id = st.id;
                        x = st.x;
                        a = st.a;
                    }
                    return! runState14 st
                | _ -> failwith "unexpected label"
        }
    and runState14 (st: State14) : Async<unit> =
        async {
            do! comms.send_string S "cancel"
            let _ = callbacks.state14Onsendcancel st
            do! comms.send_unit S _
            let st : State10 = ()
            return! runState10 st
        }
    and runState15 (st: State15) : Async<unit> =
        async {
            do! comms.send_string S "buy"
            let _ = callbacks.state15Onsendbuy st
            do! comms.send_unit S _
            let st : State10 = ()
            return! runState10 st
        }
    runState9 ()
