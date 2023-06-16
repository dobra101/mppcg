:- module(btypes, [
    is_list/1,
    get/3,
    empty/1,
    update/4,
    updateFunction/4,
    mppcg_member/2,
    mppcg_member_list/2,
    mppcg_subset/2,
    mppcg_subsetStrict/2,
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
    mppcg_callFunction/3,
    mppcg_callFunction_singleValue/3,
    mppcg_listSum/2,
    mppcg_listProduct/2,
    mppcg_minus/3,
    mppcg_succ/2,
    mppcg_pred/2,
    mppcg_generalConcat/3,
    mppcg_inverse/2
    ]).

:- use_module(library(ordsets)).
:- use_module(runCfg).

is_list([]).
is_list([_ | _]).

get(State, Key, Value) :-
    ord_fetch(Key, State, Value).

% TODO: use faster search algorithm
ord_fetch(_, [], _) :- fail, !.
ord_fetch(Key, [Key-Value | _], Value) :- !.
ord_fetch(Key, [_ | Tail], Value) :-
    ord_fetch(Key, Tail, Value).

% TODO: not both possible
ord_fetchAll(_, [], _) :- fail, !.
ord_fetchAll(Key, [Key-Value | _], Value).
ord_fetchAll(Key, [_ | Tail], Value) :-
    ord_fetchAll(Key, Tail, Value).


empty([]).


update(Key, Value, List, NewList) :-
    functor(Key, Functor, 1),
    arg(1, Key, Parameter),
    !,
    get(List, Functor, Relation),
    update(Parameter, Value, Relation, NewRelation),
    update(Functor, NewRelation, List, NewList).

update(Key, Value, List, NewList) :-
    ord_fetch(Key, List, OldValue),
    !,
    ordsets:ord_del_element(List, Key-OldValue, ListWithoutOld),
    ordsets:ord_add_element(ListWithoutOld, Key-Value, NewList).
update(Key, Value, List, NewList) :-
    ordsets:ord_add_element(List, Key-Value, NewList).

%%%%%%%%%%%%%%%%%%%%%%%%% HELPER %%%%%%%%%%%%%%%%%%%%%%%%%
between(A, B, Element) :-
    var(Element),
    gen_int_between(A, B, Element).
between(A, infinite, Element) :-
    !,
    integer(Element),
    Element >= A.
between(A, B, Element) :-
    integer(Element),
    Element >= A,
    Element =< B.

gen_int_between(-infinite, infinite, Start) :-
    gen_int_between(0, infinite, Int),
    (Start is Int; Start is -Int).
gen_int_between(Start, infinite, Start) :- Start \= -infinite.
gen_int_between(Start, End, Start) :-
    Start \= -infinite,
    End \= infinite,
    Start =< End.
gen_int_between(Start, infinite, Int) :-
    Start \= -infinite,
    Start1 is Start + 1,
    gen_int_between(Start1, infinite, Int).
gen_int_between(Start, End, Int) :-
    Start \= -infinite,
    End \= infinite,
    Start =< End,
    Start1 is Start + 1,
    gen_int_between(Start1, End, Int).

mppcg_expandEntries(_, [], []).
mppcg_expandEntries(X, [Head | Tail], [(X-[Head]) | OtherTail]) :-
    mppcg_expandEntries(X, Tail, OtherTail).

subtractSets(Set2, Set1, Difference) :-
    ordsets:ord_subtract(Set1, Set2, Difference).


lastElementOfList([Last], Last).
lastElementOfList([_ | Tail], Last) :-
    lastElementOfList(Tail, Last).

flatten(L, Flat) :-
    flatten(L, [], Flat).

flatten([], Acc, Acc) :- !.
flatten([Head | Tail], Acc, Flat) :-
    (is_list(Head) -> append(Head, Acc, NewAcc); append([Head], Acc, NewAcc)),
    flatten(Tail, NewAcc, Flat).

sortIfList(X, X) :-
    var(X),
    !.
sortIfList([], []) :- !.
sortIfList([Head | Tail], Sorted) :-
    ordsets:list_to_ord_set([Head | Tail], Sorted).
%%%%%%%%%%%%%%%%%%%%%%%%% HELPER END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% MEMBER %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_member(_, Y) :- var(Y), !, fail.
mppcg_member(X, 'BOOL') :-
    (X = true; X = false),
    !.
mppcg_member(X, Y) :-
    resolve(X, X1),
    resolve(Y, Y1),
    !,
    mppcg_member_(X1, Y1).

mppcg_member_(X, 'INTEGER') :-
    !,
    var(X) -> gen_int_between(-infinite, infinite, X); number(X).
mppcg_member_(X, 'NATURAL') :-
    !,
    var(X) -> gen_int_between(0, infinite, X); between(0, infinite, X).
mppcg_member_(Set, function(From, To)) :-
    !,
    nonvar(From),
    nonvar(To),
    mppcg_member_function(Set, From, To).
mppcg_member_(Element, [X-Y | Tail]) :-
    nonvar(Element),
    ordsets:is_ordset([X-Y | Tail]),
    !,
    mppcg_member_ordlist(Element, [X-Y | Tail]).
mppcg_member_(Element, List) :-
    !,
    mppcg_member_list(Element, List).

mppcg_member_function([], _, _) :- !.
mppcg_member_function([Head | Tail], From, To) :-
    !,
    mppcg_domain([Head | Tail], Domain),
    mppcg_range([Head | Tail], Range),
    mppcg_subset(Domain, From),
    mppcg_subset(Range, To).
mppcg_member_function((X-Y), From, To) :-
    mppcg_member_(X, From),
    mppcg_member_(Y, To).

mppcg_member_list(_, []) :- fail, !.
mppcg_member_list(X, [Head | _]) :-
    mppcg_equal(X, Head).
mppcg_member_list(X, [_ | Tail]) :-
    mppcg_member_list(X, Tail).

mppcg_member_ordlist(_, []) :- fail, !.
mppcg_member_ordlist(Element, [Element | _]) :- !.
mppcg_member_ordlist(Element, [Head | Tail]) :-
    compare(>, Element, Head),
    mppcg_member_ordlist(Element, Tail).

%%%%%%%%%%%%%%%%%%%%%%%%% MEMBER END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% SUBSET %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_subset(X, Y) :-
    resolve(X, X1),
    resolve(Y, Y1),
    sortIfList(X1, XSorted),
    sortIfList(Y1, YSorted),
    is_list(Y1) -> mppcg_subset_(XSorted, YSorted); mppcg_subset_infiniteSet(YSorted, XSorted).

mppcg_subset_([], []).
mppcg_subset_([Head | NewTail], [Head | Tail]) :-
    mppcg_subset_(NewTail, Tail).
mppcg_subset_(NewTail, [_ | Tail]) :-
    mppcg_subset_(NewTail, Tail).

mppcg_subset_infiniteSet(_, []).
mppcg_subset_infiniteSet(InfiniteSet, [Head | Tail]) :-
    mppcg_member_(Head, InfiniteSet),
    mppcg_subset_infiniteSet(InfiniteSet, Tail).

mppcg_subsetStrict(X, Y) :-
    resolveNonVar(X, X1),
    resolveNonVar(Y, Y1),
    X1 \= Y1,
    is_list(Y1) -> ordsets:ord_subset(X1, Y1); mppcg_subset_infiniteSet(Y1, X1).
%%%%%%%%%%%%%%%%%%%%%%%%% SUBSET END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% SET SUBTRACTION %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_setSubtraction(Left, empty, Left) :- !.
mppcg_setSubtraction(empty, _, empty) :- !.
mppcg_setSubtraction(Left, Right, Result) :-
    resolveNonVar(Left, L),
    resolveNonVar(Right, R),
    ordsets:list_to_ord_set(L, L1),
    ordsets:list_to_ord_set(R, R1),
    ordsets:ord_subtract(L1, R1, Result).
%%%%%%%%%%%%%%%%%%%%%%%%% SET SUBTRACTION END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% SET INTERSECTION %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_setIntersection(Left, Right, Intersection) :-
    resolveNonVar(Left, L),
    resolveNonVar(Right, R),
    ordsets:list_to_ord_set(L, L1),
    ordsets:list_to_ord_set(R, R1),
    ordsets:ord_intersection(L1, R1, Intersection).
%%%%%%%%%%%%%%%%%%%%%%%%% SET INTERSECTION END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% SET UNION %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_setUnion(Set, [], Set) :- !.
mppcg_setUnion([], Set, Set) :- !.
mppcg_setUnion(Set1, Set2, Union) :-
    resolveNonVar(Set1, L),
    resolveNonVar(Set2, R),
    ordsets:list_to_ord_set(L, L1),
    ordsets:list_to_ord_set(R, R1),
    ordsets:ord_union(L1, R1, Union).
%%%%%%%%%%%%%%%%%%%%%%%%% SET UNION END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% RELATIONS %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_domain(List, Ordered) :-
    mppcg_domain_(List, Domain),
    ordsets:list_to_ord_set(Domain, Ordered).
mppcg_domain_([], []) :- !.
mppcg_domain_([Key-_ | Tail], [Key | NewTail]) :-
    mppcg_domain_(Tail, NewTail).

mppcg_range(List, Ordered) :-
    mppcg_range_(List, Range),
    ordsets:list_to_ord_set(Range, Ordered).
mppcg_range_([], []) :- !.
mppcg_range_([_-Value | Tail], [Value | NewTail]) :-
    mppcg_range_(Tail, NewTail).

mppcg_reverse(List, Reversed) :-
    mppcg_reverse(List, [], Reversed).

mppcg_reverse([], Acc, Acc).
mppcg_reverse([Head | Tail], Acc, Result) :-
    mppcg_reverse(Tail, [Head | Acc], Result).

mppcg_inverse(Set, Inversed) :-
    mppcg_inverse_(Set, Result),
    ordsets:list_to_ord_set(Result, Inversed).

mppcg_inverse_([], []).
mppcg_inverse_([X-Y | Tail], [Y-X | NewTail]) :-
    mppcg_inverse_(Tail, NewTail).


mppcg_domainRestriction(Domain, List, Result) :-
    ordsets:list_to_ord_set(Domain, DomainOrdSet),
    findall(X-Y,
        (
            ord_fetchAll(X, List, Y),
            ordsets:ord_member(X, DomainOrdSet)
        ),
        Result
    ).


mppcg_domainSubtraction(Domain, List, Result) :-
    ordsets:list_to_ord_set(Domain, DomainOrdSet),
    findall(X-Y,
        (
            ord_fetchAll(X, List, Y),
            \+ ordsets:ord_member(X, DomainOrdSet)
        ),
        Result
    ).

mppcg_rangeRestriction(List, Range, Result) :-
    ordsets:list_to_ord_set(Range, RangeOrdSet),
    findall(X-Y,
        (
            ord_fetchAll(X, List, Y),
            ordsets:ord_member(Y, RangeOrdSet)
        ),
        Result
    ).

mppcg_rangeSubtraction(List, Range, Result) :-
    ordsets:list_to_ord_set(Range, RangeOrdSet),
    findall(X-Y,
        (
            ord_fetchAll(X, List, Y),
            \+ ordsets:ord_member(Y, RangeOrdSet)
        ),
        Result
    ).


mppcg_image(List, Set, Image) :-
    resolve(Set, S),
    findall(Res,
        (
            member(M, S),
            ord_fetchAll(M, List, Res)
        ),
        Result
    ),
    ordsets:list_to_ord_set(Result, Image).


mppcg_override(List1, List2, Result) :-
    mppcg_domain(List2, Domain),
    mppcg_domainSubtraction(Domain, List1, Subtracted),
    mppcg_relationUnion(List2, Subtracted, Result).


mppcg_relationUnion(List1, List2, Result) :-
    ordsets:list_to_ord_set(List1, Ord1),
    ordsets:list_to_ord_set(List2, Ord2),
    ordsets:ord_union(Ord1, Ord2, Result).


mppcg_forwardComposition(P, Q, Result) :-
    findall(
        (X-Y),
        (
            ord_fetchAll(X, P, XValue),
            member(Z, XValue),
            ord_fetchAll(Z, Q, Y)
        ),
        Result
    ).


mppcg_mult(Left, Right, List) :-
    resolve(Left, L),
    resolve(Right, R),
    findall(X-Y, (mppcg_member_list(X, L), mppcg_member_list(Y, R)), Result),
    ordsets:list_to_ord_set(Result, List).

mppcg_callFunction(List, [Param1 | Tail], Result) :-
    !,
    findall(Res,
        (
            member(Param, [Param1 | Tail]),
            ord_fetch(Param, List, Res)
        ),
        Result
    ).
mppcg_callFunction(List, Parameter, Result) :-
    ord_fetch(Parameter, List, Result).

mppcg_generalConcat(First, Set, Concat) :-
    findall(M, mppcg_member(M, Set), List),
    append([First], List, All),
    ordsets:list_to_ord_set(All, OrdSet),
    mppcg_generalConcat(OrdSet, Concat).
mppcg_generalConcat([], []).
mppcg_generalConcat([(_-X) | Tail], Concat) :-
    mppcg_generalConcat(Tail, Rest),
    append(X, Rest, Concat).
%%%%%%%%%%%%%%%%%%%%%%%%% RELATIONS END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% SETS %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_powerSet(Set, PowerSet) :-
    findall(Subset, mppcg_subset(Subset, Set), PW),
    ordsets:list_to_ord_set(PW, PowerSet).

mppcg_powerSet1(Set, PowerSet) :-
    mppcg_powerSet(Set, PW),
    ordsets:ord_del_element(PW, [], PowerSet).
%%%%%%%%%%%%%%%%%%%%%%%%% SETS END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% LISTS %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_max(X, Max) :-
    resolve(X, X1),
    ordsets:list_to_ord_set(X1, OrdSet),
    lastElementOfList(OrdSet, Max).

mppcg_min(X, Min) :-
    resolve(X, X1),
    ordsets:list_to_ord_set(X1, [Min | _]).

mppcg_card(X, Card) :-
    length(X, Card).
%%%%%%%%%%%%%%%%%%%%%%%%% LISTS END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% SEQUENCES %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_sequenceFront(Sequence, Front) :-
    append(Front, [_], Sequence), !.


mppcg_sequenceTail([_ | Tail], Tail).


mppcg_sequenceFirst([First | _], First).


mppcg_sequenceLast(Sequence, Last) :-
    lastElementOfList(Sequence, Last).


mppcg_sequenceRestrictFront([], _, []) :- !.
mppcg_sequenceRestrictFront(_, 0, []) :- !.
mppcg_sequenceRestrictFront([Head | Tail], N, [Head | NewTail]) :-
    N1 is N - 1,
    mppcg_sequenceRestrictFront(Tail, N1, NewTail).


mppcg_sequenceRestrictTail(Sequence, 0, Sequence) :- !.
mppcg_sequenceRestrictTail([_ | Tail], N, Restricted) :-
    N1 is N - 1,
    mppcg_sequenceRestrictTail(Tail, N1, Restricted).


mppcg_sequenceReverse([], []).
mppcg_sequenceReverse([Head | Tail], Reversed) :-
    mppcg_sequenceReverse(Tail, ReversedTail),
    append(ReversedTail, [Head], Reversed).
%%%%%%%%%%%%%%%%%%%%%%%%% SEQUENCES END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% EQUAL %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_equal(X, Y) :-
    resolve(X, X1),
    resolve(Y, Y1),
    mppcg_equal_(X1, Y1),
    !.

mppcg_equal_(X, X) :- !.
mppcg_equal_(X, Y) :-
    number(X),
    number(Y),
    !,
    X =< Y,
    Y =< X.
mppcg_equal_(List1, List2) :-
    is_list(List1),
    is_list(List2),
    ordsets:list_to_ord_set(List1, Ord1),
    ordsets:list_to_ord_set(List2, Ord2),
    (Ord1 = Ord2; mppcg_equal_nestedSets(Ord1, Ord2)),
    !.


mppcg_equal_nestedSets([], []).
mppcg_equal_nestedSets([[Head1 | Tail1] | OuterTail1], [[Head2 | Tail2] | OuterTail2]) :-
    mppcg_equal([Head1 | Tail1], [Head2 | Tail2]),
    mppcg_equal_nestedSets(OuterTail1, OuterTail2).
mppcg_equal_nestedSets([Head1 | Tail1], [Head2 | Tail2]) :-
    \+ is_list(Head1),
    \+ is_list(Head2),
    mppcg_equal(Head1, Head2),
    mppcg_equal_nestedSets(Tail1, Tail2).


mppcg_notEqual(X, Y) :-
    resolve(X, X1),
    resolve(Y, Y1),
    mppcg_notEqual_(X1, Y1),
    !.

mppcg_notEqual_(X, X) :- !, fail.
mppcg_notEqual_(X, Y) :-
    number(X),
    number(Y),
    !,
    (X < Y ; X > Y).
mppcg_notEqual_(SetOrInterval1, SetOrInterval2) :-
    (mppcg_member(X, SetOrInterval1), \+ mppcg_member(X, SetOrInterval2));
    (mppcg_member(X, SetOrInterval2), \+ mppcg_member(X, SetOrInterval1)), !.
mppcg_notEqual_(X, Y) :- X \= Y, !.
%%%%%%%%%%%%%%%%%%%%%%%%% EQUAL END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% ARITHMETIC %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_pow(X, Y, PowInt) :-
    mppcg_pow_(X, Y, 1, Pow),
    PowInt is integer(Pow),
    mppcg_equal(Pow, PowInt),
    !.
mppcg_pow(X, Y, Pow) :-
    mppcg_pow_(X, Y, 1, Pow).

mppcg_pow_(_, 0, Acc, Acc) :- !.
mppcg_pow_(X, Y, Acc, Result) :-
    NewAcc is Acc * X,
    Y1 is Y - 1,
    mppcg_pow_(X, Y1, NewAcc, Result).


mppcg_listSum(List, Sum) :-
    mppcg_listSum(List, 0, Sum).

mppcg_listSum([], Acc, Acc).
mppcg_listSum([Head | Tail], Acc, Sum) :-
    Acc1 is Acc + Head,
    mppcg_listSum(Tail, Acc1, Sum).


mppcg_listProduct(List, Prod) :-
    mppcg_listProduct(List, 1, Prod).
mppcg_listProduct([], Acc, Acc).
mppcg_listProduct([Head | Tail], Acc, Prod) :-
    Acc1 is Acc * Head,
    mppcg_listProduct(Tail, Acc1, Prod).


mppcg_minus(X, Y, Result) :-
    ordsets:is_ordset(X),
    ordsets:is_ordset(Y),
    !,
    ordsets:ord_subtract(X, Y, Result).
mppcg_minus(List1, List2, Result) :-
    findall(X, (mppcg_member(X, List1), \+ mppcg_member(X, List2)), Result).


mppcg_pred(X, Pred) :- Pred is X - 1.
mppcg_succ(X, Succ) :- Succ is X + 1.
%%%%%%%%%%%%%%%%%%%%%%%%% ARITHMETIC END %%%%%%%%%%%%%%%%%%%%%%%%%



resolveNonVar(X, Y) :-
    nonvar(X),
    resolve(X, Y).

resolve(X, X) :- var(X).
resolve('NAT', List) :- maxInt(Max), resolve_interval((0, Max), List), !.
resolve('NAT1', List) :- maxInt(Max), resolve_interval((1, Max), List), !.
resolve('INT', List) :- minInt(Min), maxInt(Max), resolve_interval((Min, Max), List), !.
resolve('BOOL', [true, false]) :- !.
resolve((A, B), List) :- resolve_interval((A, B), List), !.
resolve(function(From, To), function(F, T)) :-
    resolve(From, F),
    resolve(To, T).
resolve([Head | Tail], List) :- resolve_list([Head | Tail], List), !.
resolve(X, X).
resolve_interval((A, B), List) :-
    nonvar(A),
    nonvar(B),
    !,
    findall(X, between(A, B, X), List).
resolve_couple(X-Y, X-Y).
resolve_list([], []).
resolve_list([X-Y | Tail], Result) :-
    !,
    mppcg_expandEntries(X, Y, Expanded),
    resolve_list(Tail, NewTail),
    append(Expanded, NewTail, Result).
resolve_list(List, List).