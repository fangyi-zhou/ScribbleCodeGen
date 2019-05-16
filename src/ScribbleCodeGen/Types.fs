namespace ScribbleCodeGen

type Role = string
type Label = string
type State = int
type Assertion = string
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
type CFSM = State * TransitionMap

type StateVariableMap = Map<State, (Variable * VarType) list>
