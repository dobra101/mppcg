:- use_module(library(ordsets)).

modelcheck_start(States, TCount) :-
    start(S0),
    \+ prop(S0, unsafe),
    modelcheck(S0, [S0], VisitedStates, TCount),
    length(VisitedStates, SCount),
    States is SCount + 1.
modelcheck_start(_, _) :-
    print("Counterexample found").

modelcheck(State, VisitedStates, NewVisitedStates, TransitionCount) :-
    findall(T-NextState, trans(T, State, NextState), Trans),

    Transitions \= [], % deadlock check

    prune_transitions(Trans, VisitedStates, Transitions),
    no_violations(Transitions), % invariant check

    ordsets:ord_add_element(VisitedStates, State, VisitedStates1),
    modelcheck_all(Transitions, VisitedStates1, NewVisitedStates, TCount),

    length(Trans, TransCount),
    TransitionCount is TCount + TransCount.

modelcheck_all([], VisitedStates, VisitedStates, 0).
modelcheck_all([(_-State) | Tail], VisitedStates, NewVisitedStates, TransitionCount) :-
    modelcheck(State, VisitedStates, VisitedStates1, TCount),
    modelcheck_all(Tail, VisitedStates1, NewVisitedStates, TCount1),
    !,
    TransitionCount is TCount + TCount1.

prune_transitions([], _, []).
prune_transitions([(_-State) | Tail], VisitedStates, NewTail) :-
    ordsets:ord_member(State, VisitedStates),
    !,
    prune_transitions(Tail, VisitedStates, NewTail).
prune_transitions([Head | Tail], VisitedStates, [Head | NewTail]) :-
    prune_transitions(Tail, VisitedStates, NewTail).

no_violations([]).
no_violations([(Transition-State) | Tail]) :-
    \+ prop(State, unsafe),
    !,
    no_violations(Tail).