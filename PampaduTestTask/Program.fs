type State =
    | CLOSED
    | LISTEN
    | SYN_SENT
    | SYN_RCVD
    | ESTABLISHED
    | CLOSE_WAIT
    | LAST_ACK
    | FIN_WAIT_1
    | FIN_WAIT_2
    | CLOSING
    | TIME_WAIT
    | ERROR

type Event =
    | APP_PASSIVE_OPEN
    | APP_ACTIVE_OPEN
    | APP_SEND
    | APP_CLOSE
    | APP_TIMEOUT
    | RCV_SYN
    | RCV_ACK
    | RCV_SYN_ACK
    | RCV_FIN
    | RCV_FIN_ACK

type StateTransition(currentState: State, event: Event) =
    member val CurrentState = currentState
    member val Event = event

type Machine() =
    let mutable currentState = State.CLOSED
    let transitions =
        [
            (State.CLOSED, Event.APP_PASSIVE_OPEN), State.LISTEN
            (State.CLOSED, Event.APP_ACTIVE_OPEN), State.SYN_SENT
            (State.LISTEN, Event.RCV_SYN), State.SYN_RCVD
            (State.LISTEN, Event.APP_SEND), State.SYN_SENT
            (State.LISTEN, Event.APP_CLOSE), State.CLOSED
            (State.SYN_RCVD, Event.APP_CLOSE), State.FIN_WAIT_1
            (State.SYN_RCVD, Event.RCV_ACK), State.ESTABLISHED
            (State.SYN_SENT, Event.RCV_SYN), State.SYN_RCVD
            (State.SYN_SENT, Event.RCV_SYN_ACK), State.ESTABLISHED
            (State.SYN_SENT, Event.APP_CLOSE), State.CLOSED
            (State.ESTABLISHED, Event.APP_CLOSE), State.FIN_WAIT_1
            (State.ESTABLISHED, Event.RCV_FIN), State.CLOSE_WAIT
            (State.FIN_WAIT_1, Event.RCV_FIN), State.CLOSING
            (State.FIN_WAIT_1, Event.RCV_FIN_ACK), State.TIME_WAIT
            (State.FIN_WAIT_1, Event.RCV_ACK), State.FIN_WAIT_2
            (State.CLOSING, Event.RCV_ACK), State.TIME_WAIT
            (State.FIN_WAIT_2, Event.RCV_FIN), State.TIME_WAIT
            (State.TIME_WAIT, Event.APP_TIMEOUT), State.CLOSED
            (State.CLOSE_WAIT, Event.APP_CLOSE), State.LAST_ACK
            (State.LAST_ACK, Event.RCV_ACK), State.CLOSED
        ] |> Map.ofList

    member this.CurrentState
        with get() = currentState
        and set(value) = currentState <- value

    member this.GetNextState (event: Event) =
        let transition = currentState, event
        match transitions.TryGetValue transition with
        | true, nextState -> nextState
        | false, _ -> State.ERROR

    member this.ChangeState (event: Event) =
        currentState <- this.GetNextState event
        currentState

let userEvents = [Event.APP_PASSIVE_OPEN; Event.APP_SEND; Event.RCV_SYN_ACK]
let userEvents2 = [Event.APP_ACTIVE_OPEN]
let userEvents3 = [Event.APP_ACTIVE_OPEN; Event.RCV_SYN_ACK; Event.APP_CLOSE; Event.RCV_FIN_ACK; Event.RCV_ACK]

let machine = Machine()
for userEvent in userEvents3 do
    machine.ChangeState(userEvent) |> ignore

printfn "%s" (machine.CurrentState.ToString())