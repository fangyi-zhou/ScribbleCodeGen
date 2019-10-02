module ScribbleGeneratedTwoBuyerS
(* This file is GENERATED, do not modify manually *)
open FluidTypes.Annotations

type Role =
    | A
    | B
type State10 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
}
and State11 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{y:int|((x) = (y))}")>] y : int
}
and State7 = unit
and State8 = unit
and State9 = {
    id : int
}
type CallbacksS = {
    [<Refined("(st: State7) -> (id: int) -> unit")>] state7OnreceivebookId : State7 -> int -> unit
    [<Refined("(st: State9) -> {x:int|((x) >= (0))}")>] state9OnsendquoteA : State9 -> int
    [<Refined("(st: State10) -> {y:int|(((st)$x) = (y))}")>] state10OnsendquoteB : State10 -> int
    [<Refined("(st: State11) -> unit")>] state11Onreceivebuy : State11 -> unit
    [<Refined("(st: State11) -> unit")>] state11Onreceivecancel : State11 -> unit
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
    let rec runState7 (st: State7) : Async<unit> =
        async {
            let! label = comms.recv_string A ()
            match label with
                | "bookId" ->
                    let! id = comms.recv_int A ()
                    callbacks.state7OnreceivebookId st id
                    let st : State9 = {
                        id = id;
                    }
                    return! runState9 st
                | _ -> failwith "unexpected label"
        }
    and runState8 (st: State8) : Async<unit> =
        async {
            ()
        }
    and runState9 (st: State9) : Async<unit> =
        async {
            do! comms.send_string A "quoteA"
            let x = callbacks.state9OnsendquoteA st
            do! comms.send_int A x
            let st : State10 = {
                id = st.id;
                x = x;
            }
            return! runState10 st
        }
    and runState10 (st: State10) : Async<unit> =
        async {
            do! comms.send_string B "quoteB"
            let y = callbacks.state10OnsendquoteB st
            do! comms.send_int B y
            let st : State11 = {
                id = st.id;
                x = st.x;
                y = y;
            }
            return! runState11 st
        }
    and runState11 (st: State11) : Async<unit> =
        async {
            let! label = comms.recv_string A ()
            match label with
                | "buy" ->
                    let! _dum1 = comms.recv_unit A ()
                    callbacks.state11Onreceivebuy st 
                    let st : State8 = ()
                    return! runState8 st
                | "cancel" ->
                    let! _dum3 = comms.recv_unit A ()
                    callbacks.state11Onreceivecancel st 
                    let st : State8 = ()
                    return! runState8 st
                | _ -> failwith "unexpected label"
        }
    runState7 ()
