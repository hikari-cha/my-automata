Require Import Stdlib.Lists.List.

Section DFA_Definition.
    Variable State Symbol : Type.
    Record DFA := {
        transition : State -> Symbol -> State;
        initialState : State;
        isFinalState : State -> bool
    }.

    Definition runDFA (dfa : DFA) (input : list Symbol) : State
        := fold_left (transition dfa)  input (initialState dfa). 
    
    Definition isAccepted (dfa : DFA) (input : list Symbol) : bool
        := isFinalState dfa (runDFA dfa input).
    
End DFA_Definition.

Arguments runDFA {State Symbol}.
Arguments isAccepted {State Symbol}.
