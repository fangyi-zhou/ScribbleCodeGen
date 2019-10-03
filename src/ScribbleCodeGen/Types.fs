namespace ScribbleCodeGen

open FluidTypes.Refinements

type CodeGenMode =
    | LegacyApi
    | EventApi
    | FStar

type Role = string
type Label = string
type State = int
type Assertion = Term list
type Action =
    | Request
    | Accept
    | Send
    | Receive
type Variable = string
type VarType = string
type PayloadItem = Variable * VarType
type Payload = PayloadItem list
type Transition = {
    fromState: State;
    toState: State;
    partner: Role;
    action: Action;
    label: Label;
    payload: Payload
    assertion: Assertion;
}

type TransitionMap = Map<State, Transition list>
type CFSM = (* init *) State * (* terminal *) State list * TransitionMap

type Refinement = string
type StateVariableMap = Map<State, (Variable * VarType) list * Assertion>
