module ScribbleGeneratedTwoBuyerS
(* This file is GENERATED, do not modify manually *)
open FluidTypes.Annotations

type Role =
    | A
    | B
type State35 = unit
and State36 = unit
and State37 = {
    id : int
}
and State38 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
}
and State39 = {
    id : int
    [<Refined("{x:int|((x) >= (0))}")>] x : int
    [<Refined("{y:int|((x) = (y))}")>] y : int
}
type CallbacksS = {
    [<Refined("(st: State35) -> (id: int) -> unit")>] state35OnreceivebookId : State35 -> int -> unit
    [<Refined("(st: State37) -> {x:int|((x) >= (0))}")>] state37OnsendquoteA : State37 -> int
    [<Refined("(st: State38) -> {y:int|(((st)$x) = (y))}")>] state38OnsendquoteB : State38 -> int
    [<Refined("(st: State39) -> unit")>] state39Onreceivebuy : State39 -> unit
    [<Refined("(st: State39) -> unit")>] state39Onreceivecancel : State39 -> unit
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
    let rec runState35 (st: State35) : Async<unit> =
        async {
            let! label = comms.recv_string A ()
            match label with
                | "bookId" ->
                    let! id = comms.recv_int A ()
                    callbacks.state35OnreceivebookId st id
                    let st : State37 = {
                        id = id;
                    }
                    return! runState37 st
                | _ -> failwith "unexpected label"
        }
    and runState36 (st: State36) : Async<unit> =
        async {
            ()
        }
    and runState37 (st: State37) : Async<unit> =
        async {
            do! comms.send_string A "quoteA"
            let x = callbacks.state37OnsendquoteA st
            do! comms.send_int A x
            let st : State38 = {
                id = st.id;
                x = x;
            }
            return! runState38 st
        }
    and runState38 (st: State38) : Async<unit> =
        async {
            do! comms.send_string B "quoteB"
            let y = callbacks.state38OnsendquoteB st
            do! comms.send_int B y
            let st : State39 = {
                id = st.id;
                x = st.x;
                y = y;
            }
            return! runState39 st
        }
    and runState39 (st: State39) : Async<unit> =
        async {
            let! label = comms.recv_string A ()
            match label with
                | "buy" ->
                    let! _ = comms.recv_unit A ()
                    callbacks.state39Onreceivebuy st 
                    let st : State36 = ()
                    return! runState36 st
                | "cancel" ->
                    let! _ = comms.recv_unit A ()
                    callbacks.state39Onreceivecancel st 
                    let st : State36 = ()
                    return! runState36 st
                | _ -> failwith "unexpected label"
        }
    runState35 ()
