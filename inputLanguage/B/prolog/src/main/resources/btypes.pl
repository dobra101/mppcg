:- module(btypes, [
    is_list/1,
    get/3,
    empty/1,
    update/4,
    avl2List/2,
    updateFunction/4,
    mppcg_member/2,
    mppcg_member_list/2,
    mppcg_subset/2,
    mppcg_setSubtraction/3,
    mppcg_setIntersection/3,
    mppcg_setUnion/3,
    mppcg_domain/2,
    mppcg_range/2,
    mppcg_reverse/2,
    mppcg_domainSubtraction/3,
    mppcg_domainRestriction/3,
    mppcg_image/3,
    mppcg_override/3,
    mppcg_rangeSubtraction/3,
    mppcg_rangeRestriction/3,
    mppcg_powerSet/2,
    mppcg_powerSet1/2,
    mppcg_max/2,
    mppcg_min/2,
    mppcg_card/2,
    mppcg_sequenceFront/2,
    mppcg_sequenceTail/2,
    mppcg_sequenceFirst/2,
    mppcg_sequenceLast/2,
    mppcg_sequenceRestrictFront/3,
    mppcg_sequenceRestrictTail/3,
    mppcg_sequenceReverse/2,
    mppcg_forwardComposition/3,
    mppcg_mult/3,
    mppcg_equal/2,
    mppcg_notEqual/2,
    mppcg_pow/3,
    mppcg_callFunction/3
    ]).

:- use_module(library(avl)).

is_list([]).
is_list([_ | Tail]) :-
    is_list(Tail).

% AVL
get(State, Key, Value) :-
    avl:avl_fetch(Key, State, Value).

empty(T) :- avl:empty_avl(T).

update(Key, Value, AVL, NewAVL) :-
    functor(Key, Functor, 1),
    arg(1, Key, Parameter),
    get(AVL, Functor, set(FuncRel)),
    updateFunction(FuncRel, Parameter, Value, NewValue),
    avl:avl_store(Functor, AVL, set(NewValue), NewAVL),
    !.
update(Key, Value, AVL, NewAVL) :-
    avl:avl_store(Key, AVL, Value, NewAVL).

avl2List(AVL, List) :-
    avl:avl_to_list(AVL, List).

updateFunction([], Parameter, Value, [Parameter/Value]).
updateFunction([Parameter/_ | Tail], Parameter, Value, [Parameter/Value | Tail]) :- !.
updateFunction([K/V | Tail], Parameter, Value, [K/V | NewTail]) :-
    updateFunction(Tail, Parameter, Value, NewTail).

% Operators
% TODO: function: check if e.g. only one solution is found
mppcg_member(set(Set), pow(X)) :-
    findall(_, (mppcg_member(E, Set), \+ mppcg_member(E, X)), []).
mppcg_member(true, 'BOOL').
mppcg_member(false, 'BOOL').
mppcg_member(Element, 'INT') :-
    integer(Element).
mppcg_member(Element, 'NAT') :-
    integer(Element),
    between(0, infinite, Element).
mppcg_member(Element, (A, B)) :-
    between(A, B, Element).
mppcg_member(Set, function(From, To)) :-
    findall(_, (mppcg_member(E, Set), \+ mppcg_member(E, function(From, To))), []).
mppcg_member(set(Set), function(From, To)) :-
    findall(_, (mppcg_member(E, Set), \+ mppcg_member(E, function(From, To))), []).
mppcg_member((X/Y), function(From, To)) :-
    mppcg_member(X, From),
    mppcg_member(Y, To).
mppcg_member(Element, set(Set)) :-
    mppcg_member(Element, Set).
mppcg_member(Element, List) :-
    is_list(List),
    mppcg_member_list(Element, List).

mppcg_member_list(_, []) :- fail.
mppcg_member_list(X, [Head | _]) :-
    mppcg_equal(X, Head).
mppcg_member_list(X, [_ | Tail]) :-
    mppcg_member_list(X, Tail).

between(A, B, Element) :-
    var(Element),
    gen_int_between(A, B, Element).
between(A, B, Element) :-
    integer(Element),
    Element >= A,
    (B \= infinite -> Element =< B; true).

gen_int_between(Start, infinite, Start).
gen_int_between(Start, End, Start) :-
    End \= infinite,
    Start =< End.
gen_int_between(Start, infinite, Int) :-
    Start1 is Start + 1,
    gen_int_between(Start1, infinite, Int).
gen_int_between(Start, End, Int) :-
    End \= infinite,
    Start =< End,
    Start1 is Start + 1,
    gen_int_between(Start1, End, Int).

mppcg_subset(set(Left), set(Right)) :-
    findall(X, (member(X, Left), member(X, Right)), Left). % TODO: use mppcg_member here?
mppcg_subset([], []).
mppcg_subset([Head | NewTail], [Head | Tail]) :-
    mppcg_subset(NewTail, Tail).
mppcg_subset(NewTail, [_ | Tail]) :-
    mppcg_subset(NewTail, Tail).

mppcg_setSubtraction(set(Left), set(Right), set(Result)) :-
    findall(X, (member(X, Left), \+ member(X, Right)), Res), % TODO: use mppcg_member here?
    sort(Res, Result).

mppcg_setIntersection(set(Left), set(Right), set(Intersection)) :-
    findall(X, (member(X, Left), member(X, Right)), Inter), % TODO: use mppcg_member here?
    sort(Inter, Intersection).

mppcg_setUnion(set(Left), set(Right), set(Union)) :-
    append(Left, Right, Appended),
    sort(Appended, Union).

mppcg_domain(set(Relation), set(Sorted)) :-
    mppcg_domain(Relation, Domain),
    sort(Domain, Sorted).
mppcg_domain([], []) :- !.
mppcg_domain([(X/_) | Tail], [X | NewTail]) :-
    mppcg_domain(Tail, NewTail).

mppcg_range(set(Relation), set(Sorted)) :-
    mppcg_range(Relation, Range),
    sort(Range, Sorted).
mppcg_range([], []) :- !.
mppcg_range([(_/Y) | Tail], [Y | NewTail]) :-
    mppcg_range(Tail, NewTail).

mppcg_reverse(set(L), set(L1)) :- mppcg_reverse(L, L1), !.
mppcg_reverse([], []) :- !.
mppcg_reverse([(X/Y) | Tail], [(Y/X) | NewTail]) :-
    mppcg_reverse(Tail, NewTail),
    !.

mppcg_domainRestriction(Domain, set(Set), set(Result)) :-
    mppcg_domainRestriction(Domain, Set, Result),
    !.
mppcg_domainRestriction(_, [], []) :- !.
mppcg_domainRestriction(Domain, [(X/Y) | Tail], [(X/Y) | NewTail]) :-
     member(X, Domain), % TODO: use mppcg_member here?
     mppcg_domainRestriction(Domain, Tail, NewTail),
     !.
mppcg_domainRestriction(Domain, [_ | Tail], NewTail) :-
    mppcg_domainRestriction(Domain, Tail, NewTail),
    !.

mppcg_domainSubtraction(Domain, set(Set), set(Result)) :-
    mppcg_domainSubtraction(Domain, Set, Result),
    !.
mppcg_domainSubtraction(_, [], []) :- !.
mppcg_domainSubtraction(Domain, [(X/Y) | Tail], [(X/Y) | NewTail]) :-
    \+ member(X, Domain), % TODO: use mppcg_member here?
    mppcg_domainSubtraction(Domain, Tail, NewTail),
    !.
mppcg_domainSubtraction(Domain, [_ | Tail], NewTail) :-
    mppcg_domainSubtraction(Domain, Tail, NewTail),
    !.

mppcg_image(Relation, set(Set), set(Image)) :-
    mppcg_image(Relation, Set, Image),
    !.
mppcg_image(_, [], []) :- !.
% image of set
mppcg_image(Relation, [X | Rest], Image) :-
    findall(Y, mppcg_member((X/Y), Relation), Ys),
    mppcg_image(Relation, Rest, OtherTail),
    append(Ys, OtherTail, Image),
    !.
mppcg_image(Relation, [_ | Rest], NewTail) :-
    mppcg_image(Relation, Rest, NewTail).
% image of interval
mppcg_image(Relation, (A, B), Result) :-
    findall(Element, mppcg_member(Element, (A, B)), Set),
    mppcg_image(Relation, Set, Result),
    !.

mppcg_override(Overridden, Set, Result) :-
    mppcg_domain(Set, Domain),
    mppcg_domainSubtraction(Domain, Overridden, Tail),
    ((Tail = set(T), Set = set(S)) -> append(S, T, Result); append(Set, Tail, Result)).

mppcg_rangeRestriction(Relation, set(Set), set(Result)) :-
    mppcg_rangeRestriction(Relation, Set, Result),
    !.
mppcg_rangeRestriction(set(Relation), set(Set), set(Result)) :-
    mppcg_rangeRestriction(Relation, Set, Result),
    !.
mppcg_rangeRestriction([], _, []) :- !.
mppcg_rangeRestriction([(X/Y) | Tail], Range, [(X/Y) | NewTail]) :-
    member(Y, Range), % TODO: use mppcg_member here?
    mppcg_rangeRestriction(Tail, Range, NewTail),
    !.
mppcg_rangeRestriction([_ | Tail], Range, NewTail) :-
    mppcg_rangeRestriction(Tail, Range, NewTail),
    !.

mppcg_rangeSubtraction(Relation, set(Set), set(Result)) :-
    mppcg_rangeSubtraction(Relation, Set, Result),
    !.
mppcg_rangeSubtraction([], _, []) :- !.
mppcg_rangeSubtraction([(X/Y) | Tail], Range, [(X/Y) | NewTail]) :-
    \+ member(Y, Range), % TODO: use mppcg_member here?
    mppcg_rangeSubtraction(Tail, Range, NewTail),
    !.
mppcg_rangeSubtraction([_ | Tail], Range, NewTail) :-
    mppcg_rangeSubtraction(Tail, Range, NewTail),
    !.

mppcg_powerSet('INT', pow('INT')) :- !.
mppcg_powerSet(set(Set), Powerset) :-
    mppcg_powerSet(Set, Powerset),
    !.
mppcg_powerSet(Set, set(Sorted)) :-
    findall(set(Subset), (mppcg_subset(Sub, Set), sort(Sub, Subset)), Powerset),
    sort(Powerset, Sorted),
    !.

mppcg_powerSet1('INT', pow1('INT')) :- !.
mppcg_powerSet1(set(Set), Powerset) :-
    mppcg_powerSet1(Set, Powerset),
    !.
mppcg_powerSet1(Set, set(Sorted)) :-
    findall(set(Subset), (mppcg_subset(Sub, Set), sort(Sub, Subset), Subset \= []), Powerset),
    sort(Powerset, Sorted),
    !.

mppcg_max(set(Set), Max) :-
    mppcg_max(Set, Max),
    !.
mppcg_max(Set, Max) :-
    mppcg_max(Set, unset, Max),
    !.
mppcg_max([], unset, unset) :- fail, !.
mppcg_max([Head | Tail], unset, Max) :-
    mppcg_max(Tail, Head, Max),
    !.
mppcg_max([], Acc, Acc) :- !.
mppcg_max([Head | Tail], Acc, Max) :-
    Head > Acc,
    mppcg_max(Tail, Head, Max),
    !.
mppcg_max([_ | Tail], Acc, Max) :-
    mppcg_max(Tail, Acc, Max),
    !.

mppcg_min(set(Set), Min) :-
    mppcg_min(Set, Min),
    !.
mppcg_min(Set, Min) :-
    mppcg_min(Set, unset, Min),
    !.
mppcg_min([], unset, unset) :- fail, !.
mppcg_min([Head | Tail], unset, Min) :-
    mppcg_min(Tail, Head, Min),
    !.
mppcg_min([], Acc, Acc) :- !.
mppcg_min([Head | Tail], Acc, Min) :-
    Head < Acc,
    mppcg_min(Tail, Head, Min),
    !.
mppcg_min([_ | Tail], Acc, Min) :-
    mppcg_min(Tail, Acc, Min),
    !.

mppcg_card(set(List),  Length) :-
    length(List, Length),
    !.
mppcg_card(List, Length) :-
    is_list(List),
    length(List, Length),
    !.

mppcg_sequenceFront(Sequence, Front) :-
    append(Front, [_], Sequence),
    !.

mppcg_sequenceTail([_ | Tail], Tail) :- !.

mppcg_sequenceFirst([First | _], First).

mppcg_sequenceLast([Last], Last).
mppcg_sequenceLast([_ | Tail], Last) :-
    mppcg_sequenceLast(Tail, Last).

mppcg_sequenceRestrictFront([], _, []) :- !.
mppcg_sequenceRestrictFront(_, 0, []) :- !.
mppcg_sequenceRestrictFront([Head | Tail], N, [Head | NewTail]) :-
    N1 is N - 1,
    mppcg_sequenceRestrictFront(Tail, N1, NewTail).

mppcg_sequenceRestrictTail(Sequence, N, Sequence) :-
    length(Sequence, L),
    L =< N,
    !.
mppcg_sequenceRestrictTail([_ | Tail], N, Result) :-
    mppcg_sequenceRestrictTail(Tail, N, Result).

mppcg_sequenceReverse([], []).
mppcg_sequenceReverse([Head | Tail], Reversed) :-
    mppcg_sequenceReverse(Tail, ReversedTail),
    append(ReversedTail, [Head], Reversed).

mppcg_forwardComposition(P, Q, Result) :-
    findall((X, Y),
        (mppcg_member((X/Z), P), mppcg_member((Z/Y), Q)),
        Result
    ).

mppcg_mult(Interval1, Interval2, Result) :-
    findall((X/Y),
        (mppcg_member(X, Interval1), mppcg_member(Y, Interval2)),
        Result
    ).

mppcg_equal(X, X) :- !.
mppcg_equal(X, Y) :- number(X), number(Y), !, X =< Y, X >= Y. % e.g. X = 1.0, Y = 1
mppcg_equal(set(Set), Other) :-
    findall(X, (mppcg_member(X, Set), \+ mppcg_member(X, Other)), []), !,
    findall(X, (mppcg_member(X, Other), \+ mppcg_member(X, Set)), []), !.

mppcg_notEqual(X, X) :- !, fail.
mppcg_notEqual(X, Y) :- number(X), number(Y), !, (X < Y; X > Y).
mppcg_notEqual(X, Y) :- X \= Y, !.
mppcg_notEqual(set(Set), Other) :-
    (mppcg_member(X, Set), \+ mppcg_member(X, Other));
    (mppcg_member(X, Other), \+ mppcg_member(X, Set)), !.

mppcg_pow(X, Y, PowInt) :-
    mppcg_pow_1(X, Y, 1, Pow),
    PowInt is integer(Pow),
    mppcg_equal(Pow, PowInt),
    !.
mppcg_pow(X, Y, Pow) :- mppcg_pow_1(X, Y, 1, Pow).
mppcg_pow_1(_, 0, Acc, Acc) :- !.
mppcg_pow_1(X, Y, Acc, Result) :-
    NewAcc is Acc * X,
    Y1 is Y - 1,
    mppcg_pow_1(X, Y1, NewAcc, Result).

mppcg_callFunction(Expression, Parameters, Result) :-
    is_list(Parameters),
    findall(Y, (mppcg_member(X, Parameters), mppcg_member((X/Y), Expression)), Res),
    flatten(Res, Flat),
    (Flat = [Result]; Result = Flat),
    !.
mppcg_callFunction(Expression, X, Result) :-
    \+ is_list(X),
    findall(Y, mppcg_member((X/Y), Expression), Res),
    flatten(Res, Flat),
    (Flat = [Result]; Result = Flat),
    !.

flatten(L, Res) :-
    flatten(L, [], Flat),
    (L = [set(_) | _] -> (sort(Flat, R), Res = set(R)); Res = Flat).
flatten([], Acc, Acc) :- !.
flatten([Head | Tail], Acc, Flat) :-
    (is_list(Head) -> append(Head, Acc, NewAcc); append([Head], Acc, NewAcc)),
    flatten(Tail, NewAcc, Flat).
flatten([set(Head) | Tail], Acc, Flat) :-
    (is_list(Head) -> append(Head, Acc, NewAcc); append([Head], Acc, NewAcc)),
    flatten(Tail, NewAcc, Flat).