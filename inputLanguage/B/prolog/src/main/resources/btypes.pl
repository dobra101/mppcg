:- module(btypes, [
    is_list/1,
    get/3,
    empty/1,
    update/4,
    avl2List/2,
    list2Avl/2,
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

:- use_module(library(avl)).
:- use_module(library(ordsets)).
:- use_module(runCfg).

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
    get(AVL, Functor, [FuncRel]),
    updateAVL(FuncRel, Parameter, Value, NewValue),
    % use this to get same avl representation
    %(avl:avl_fetch(Functor, AVL) -> avl:avl_delete(Functor, AVL, _, AVL1); AVL1 = AVL),
    %avl2List(AVL1, List),
    %ordsets:list_to_ord_set([Functor-[NewValue] | List], Ordered),
    %list2Avl(Ordered, NewAVL),
    avl:avl_store(Functor, AVL, [NewValue], NewAVL),
    !.
update(Key, Value, AVL, NewAVL) :-
    % use this to get same avl representation
    %(avl:avl_fetch(Key, AVL) -> avl:avl_delete(Key, AVL, _, AVL1); AVL1 = AVL),
    %avl2List(AVL1, List),
    %ordsets:list_to_ord_set([Key-[Value] | List], Ordered),
    %list2Avl(Ordered, NewAVL).
    avl:avl_store(Key, AVL, [Value], NewAVL).

list2Avl(List, AVL) :-
    collapseList(List, empty, AVL),
    !.
list2Avl(List, List).

avl2List(AVL, AVLList) :-
    avl:is_avl(AVL),
    avl:avl_map(btypes:avl2List1, AVL, Mapped),
    avl:avl_to_list(Mapped, List),
    ordsets:list_to_ord_set(List, Ordered),
    AVLList = Ordered, % needed since ProB inserts 'unsafe' as second parameter
    !.
avl2List(List, List).

avl2List1([], []).
avl2List1([Head | Tail], Result) :-
    avl2List(Head, NewHead),
    avl2List1(Tail, NewTail),
    ((is_list(NewHead), NewHead \= []) ->
    append(NewHead, NewTail, Result);
    Result = [NewHead | NewTail]
    ).

updateAVL(AVL, Key, Value, NewAVL) :-
    Key \= (_X - _Y),
    avl:avl_store(Key, AVL, [Value], NewAVL).
updateAVL(AVL, X-[Y], Value, NewAVL) :-
    avl:avl_fetch(X, AVL, [AVLAtX]),
    updateAVL(AVLAtX, Y, Value, NewAVLAtX),
    !,
    avl:avl_store(X, AVL, [NewAVLAtX], NewAVL).
updateAVL(AVL, X-[Y], Value, NewAVL) :-
    appendValueToKey(Y, Value, NewValue),
    list2Avl([NewValue], AVLAtX),
    avl:avl_store(X, AVL, [AVLAtX], NewAVL).

appendValueToKey(Y, Value, Y - [Value]) :-
    Y \= (_X - _Y1), !.
appendValueToKey(X - [Y], Value, X - [Result]) :-
    appendValueToKey(Y, Value, Result).

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

collapseList([], Acc, AVL) :-
    avl:avl_map(btypes:collapseInnerList, Acc, AVL).
collapseList([(X - Y) | Tail], Acc, Result) :-
    avl:avl_member(X, Acc, Y1),
    ordsets:ord_union(Y, Y1, YRes),
    avl:avl_store(X, Acc, YRes, NewAcc),
    !,
    collapseList(Tail, NewAcc, Result).
collapseList([(X - Y) | Tail], Acc, Result) :-
    avl:avl_store(X, Acc, Y, NewAcc),
    !,
    collapseList(Tail, NewAcc, Result).
collapseList(AVL, _, AVL) :-
    avl:is_avl(AVL).


collapseInnerList([[]], [empty]) :-
    !.
collapseInnerList([(X-Y) | Tail], [Result]) :-
    list_to_ord_set([(X-Y) | Tail], Ord),
    !,
    collapseList(Ord, empty, Result).
collapseInnerList(List, List).

collapseListOfLists([], []).
collapseListOfLists([Head | Tail], [Collapsed | NewTail]) :-
    collapseList(Head, empty, Collapsed),
    collapseListOfLists(Tail, NewTail).

deleteKeysFromAVL([], AVL, AVL).
deleteKeysFromAVL([Head | Tail], AVL, Result) :-
    avl:avl_delete(Head, AVL, _, NewAVL),
    deleteKeysFromAVL(Tail, NewAVL, Result).

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
mppcg_member(X, Y) :-
    resolve(X, X1),
    resolve(Y, Y1),
    !,
    mppcg_member_(X1, Y1).

mppcg_member_([X], 'INTEGER') :-
    !,
    mppcg_member_(X, 'INTEGER').
mppcg_member_(X, 'INTEGER') :-
    !,
    var(X) -> gen_int_between(-infinite, infinite, X); number(X).
mppcg_member_([X], 'NATURAL') :-
    !,
    mppcg_member_(X, 'NATURAL').
mppcg_member_(X, 'NATURAL') :-
    !,
    var(X) -> gen_int_between(0, infinite, X); between(0, infinite, X).
mppcg_member_(Set, function(From, To)) :-
    !,
    nonvar(From),
    nonvar(To),
    mppcg_member_function(Set, From, To).
mppcg_member_(Element, List) :-
    is_list(List),
    !,
    mppcg_member_list(Element, List).
mppcg_member_(Element, AVL) :-
    mppcg_member_avl(Element, AVL).

mppcg_member_function([Head | Tail], From, To) :-
    !,
    avl:list_to_avl([Head | Tail], AVL),
    mppcg_member_function_avl(AVL, From, To).
mppcg_member_function((X-Y), From, To) :-
    !,
    mppcg_member_(X, From),
    mppcg_member_(Y, To).
mppcg_member_function(AVL, From, To) :-
    avl:is_avl(AVL),
    mppcg_member_function_avl(AVL, From, To).

mppcg_member_function_avl(AVL, From, To) :-
    mppcg_avl_domain(AVL, Domain),
    mppcg_avl_range(AVL, Range),
    flatten(Range, RangeFlat),
    mppcg_subset(Domain, From),
    mppcg_subset(RangeFlat, To).

% use instead of avl:avl_domain
mppcg_avl_domain(AVL, Domain) :-
    findall(Key-Flat,
        (
            avl:avl_member(Key, AVL, Values), % values are either avls, or just values
            findall(Dom,
                (
                    member(V, Values),
                    avl:is_avl(V),
                    mppcg_avl_domain(V, Dom)
                ),
                ValuesDomains
            ),
            flatten(ValuesDomains, Flat)
        ),
        DomainList
    ),
    pruneDomainList(DomainList, Domain).

pruneDomainList([], []).
pruneDomainList([Head - [] | Tail], [Head | NewTail]) :-
    !,
    pruneDomainList(Tail, NewTail).
pruneDomainList([Head | Tail], [Head | NewTail]) :-
    pruneDomainList(Tail, NewTail).


% use instead of avl:avl_range
mppcg_avl_range(AVL, Range) :-
    avl:avl_range(AVL, NestedRange),
    nestedRange(NestedRange, RangeUnordered),
    ordsets:list_to_ord_set(RangeUnordered, Range).

nestedRange([], []).
nestedRange([[Head] | Tail], Range) :-
    avl:is_avl(Head),
    !,
    mppcg_avl_range(Head, HeadRange),
    nestedRange(Tail, NewTail),
    append(HeadRange, NewTail, Range).
nestedRange([[Head] | Tail], [Head | NewTail]) :-
    nestedRange(Tail, NewTail).


mppcg_member_avl(Element, AVL) :-
    \+ avl:is_avl(AVL),
    !,
    mppcg_equal(Element, AVL).
mppcg_member_avl((X-[Y]), AVL) :-
    !,
    avl:avl_member(X, AVL, Y1),
    member(YMember, Y1),
    mppcg_member_avl(Y, YMember).
mppcg_member_avl(Element, AVL) :-
    avl:avl_member(Element, AVL).

% TODO: cut?
mppcg_member_list(_, []) :- fail, !.
mppcg_member_list(X, [Head | _]) :-
    mppcg_equal(X, Head).
mppcg_member_list(X, [_ | Tail]) :-
    mppcg_member_list(X, Tail).
%%%%%%%%%%%%%%%%%%%%%%%%% MEMBER END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% SUBSET %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_subset(empty, X) :-
    avl:is_avl(X),
    !.
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
    ordsets:ord_subtract(L, R, Result).
%%%%%%%%%%%%%%%%%%%%%%%%% SET SUBTRACTION END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% SET INTERSECTION %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_setIntersection(Left, Right, Intersection) :-
    resolveNonVar(Left, L),
    resolveNonVar(Right, R),
    ordsets:ord_intersection(L, R, Intersection).
%%%%%%%%%%%%%%%%%%%%%%%%% SET INTERSECTION END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% SET UNION %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_setUnion(Left, empty, Left) :- !.
mppcg_setUnion(empty, Right, Right) :- !.
mppcg_setUnion(Left, Right, Union) :-
    resolveNonVar(Left, L),
    resolveNonVar(Right, R),
    ordsets:ord_union(L, R, Union).
%%%%%%%%%%%%%%%%%%%%%%%%% SET UNION END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% RELATIONS %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_domain(AVL, Domain) :-
    mppcg_avl_domain(AVL, Domain).


mppcg_range(AVL, Range) :-
    mppcg_avl_range(AVL, Range).
%mppcg_range(AVL, Range) :-
%    findall(Value, avl:avl_member(_, AVL, Value), Values),
%    flatten(Values, Range).


% TODO: could be optimized
mppcg_reverse(AVL, Reverse) :-
    avl:avl_to_list(AVL, List),
    mppcg_reverse_(List, Res),
    collapseList(Res, empty, Reverse).

% TODO: could be optimized
mppcg_reverse_([], []) :- !.
mppcg_reverse_([(X-Y) | Tail], Result) :-
    mppcg_expandEntries(X, Y, Entries),
    mppcg_reverse_(Tail, NewTail),
    append(Entries, NewTail, Result).


mppcg_inverse(Set, Result) :-
    mppcg_reverse(Set, Reversed),
    collapseList(Reversed, empty, Result).


mppcg_domainRestriction(Domain, AVL, Result) :-
    mppcg_avl_domain(AVL, AVLDomain),
    ordsets:list_to_ord_set(Domain, DomainOrdSet),
    ordsets:ord_intersection(DomainOrdSet, AVLDomain, Intersection),
    ordsets:ord_subtract(AVLDomain, Intersection, Difference),
    deleteKeysFromAVL(Difference, AVL, Result).


mppcg_domainSubtraction(Domain, AVL, Result) :-
    mppcg_avl_domain(AVL, AVLDomain),
    ordsets:list_to_ord_set(Domain, DomainOrdSet),
    ordsets:ord_intersection(DomainOrdSet, AVLDomain, Intersection),
    deleteKeysFromAVL(Intersection, AVL, Result).


mppcg_rangeRestriction(AVL, Range, Result) :-
    ordsets:list_to_ord_set(Range, RangeOrdSet),
    avl:avl_map(ordsets:ord_intersection(RangeOrdSet), AVL, NewAVL),
    findall(Key, avl:avl_member(Key, NewAVL, []), Keys),
    deleteKeysFromAVL(Keys, NewAVL, Result).


mppcg_rangeSubtraction(AVL, Range, Result) :-
    ordsets:list_to_ord_set(Range, RangeOrdSet),
    avl:avl_map(subtractSets(RangeOrdSet), AVL, NewAVL),
    findall(Key, avl:avl_member(Key, NewAVL, []), Keys),
    deleteKeysFromAVL(Keys, NewAVL, Result).


mppcg_image(AVL, Set, Image) :-
    resolve(Set, S),
    mppcg_image_(AVL, S, Img),
    ordsets:list_to_ord_set(Img, Image).

mppcg_image_(_, [], []).
mppcg_image_(AVL, [Head | Tail], Image) :-
    avl:avl_fetch(Head, AVL, Value),
    mppcg_image_(AVL, Tail, Img),
    append(Value, Img, Image).


mppcg_override(AVL1, AVL2, Result) :-
    mppcg_avl_domain(AVL2, Domain),
    mppcg_domainSubtraction(Domain, AVL1, Subtracted),
    mppcg_relationUnion(AVL2, Subtracted, Result).


mppcg_relationUnion(AVL1, AVL2, Result) :-
    avl:avl_to_list(AVL2, List),
    mppcg_relationUnion_(AVL1, List, Result).

mppcg_relationUnion_(AVL, [], AVL).
mppcg_relationUnion_(AVL, [(Key-Value) | Tail], Result) :-
    avl:avl_member(Key, AVL, OldValue),
    !,
    ordsets:ord_union(OldValue, Value, NewValue),
    avl:avl_store(Key, AVL, NewValue, NewAVL),
    mppcg_relationUnion_(NewAVL, Tail, Result).
mppcg_relationUnion_(AVL, [(Key-Value) | Tail], Result) :-
    avl:avl_store(Key, AVL, Value, NewAVL),
    mppcg_relationUnion_(NewAVL, Tail, Result).


mppcg_forwardComposition(P, Q, AVL) :-
    findall(
        (X-Y),
        (
            avl:avl_member(X, P, XValue),
            ordsets:ord_member(Z, XValue),
            avl:avl_member(Z, Q, Y)
        ),
        Result
    ),
    avl:list_to_avl(Result, AVL).


% TODO: multiply ASTs
mppcg_mult(Left, Right, AVL) :-
    findall(
        (X-[Y]),
        (mppcg_member(X, Left), mppcg_member(Y, Right)),
        Result
    ),
    collapseList(Result, empty, AVL).

mppcg_callFunction_singleValue(Relation, Parameters, Result) :-
    mppcg_callFunction(Relation, Parameters, [Result]).

mppcg_callFunction([Head | Tail], Parameters, Result) :-
    !,
    avl:list_to_avl([Head | Tail], AVL),
    mppcg_callFunction_avl(AVL, Parameters, Result).
mppcg_callFunction(AVL, Parameters, Result) :-
    mppcg_callFunction_avl(AVL, Parameters, Result).

mppcg_callFunction_avl(AVL, [Head | Tail], Res) :-
    !,
    mppcg_callFunction_multiParameter([Head | Tail], AVL, Res).
mppcg_callFunction_avl(AVL, Parameter, Value) :-
    avl:avl_fetch(Parameter, AVL, Value).

mppcg_callFunction_multiParameter([], _, []).
mppcg_callFunction_multiParameter([Head | Tail], AVL, Result) :-
    avl:avl_fetch(Head, AVL, Value),
    mppcg_callFunction_multiParameter(Tail, AVL, Result1),
    ordsets:ord_union(Value, Result1, Result).


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
mppcg_powerSet(AVL, PowerSet) :-
    avl:is_avl(AVL),
    !,
    findall(
        Entries,
        (
            avl:avl_member(X, AVL, Y),
            mppcg_expandEntries(X, Y, Entries)
        ),
        Expanded
    ),
    flatten(Expanded, Flat),
    mppcg_powerSet_set(Flat, PS),
    collapseListOfLists(PS, PowerSet).
mppcg_powerSet(Set, PowerSet) :-
    mppcg_powerSet_set(Set, PowerSet).


mppcg_powerSet1(AVL, PowerSet) :-
    avl:is_avl(AVL),
    !,
    findall(
        Entries,
        (
            avl:avl_member(X, AVL, Y),
            mppcg_expandEntries(X, Y, Entries)
        ),
        Expanded
    ),
    flatten(Expanded, Flat),
    mppcg_powerSet1_set(Flat, PS),
    collapseListOfLists(PS, PowerSet).
mppcg_powerSet1(Set, PowerSet) :-
    mppcg_powerSet1_set(Set, PowerSet).


mppcg_powerSet_set(Set, OrdSet) :-
    findall(Subset, mppcg_subset(Subset, Set), PowerSet),
    ordsets:list_to_ord_set(PowerSet, OrdSet).
mppcg_powerSet1_set(Set, OrdSet) :-
    mppcg_powerSet_set(Set, PowerSet),
    ordsets:ord_del_element(PowerSet, [], OrdSet).
%%%%%%%%%%%%%%%%%%%%%%%%% SETS END %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% LISTS %%%%%%%%%%%%%%%%%%%%%%%%%
mppcg_max(X, Max) :-
    resolve(X, X1),
    ordsets:list_to_ord_set(X1, OrdSet),
    lastElementOfList(OrdSet, Max).

mppcg_min(X, Min) :-
    resolve(X, X1),
    ordsets:list_to_ord_set(X1, [Min | _]).

mppcg_card(AVL, Card) :-
    avl:is_avl(AVL),
    !,
    findall(Val, avl:avl_member(_, AVL, Val), Values),
    flatten(Val, Values),
    length(Values, Card).
mppcg_card(X, Card) :-
    resolve(X, X1),
    is_list(X1),
    length(X1, Card).
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
mppcg_equal(AVL, AVL2) :-
    avl:is_avl(AVL),
    avl:is_avl(AVL2),
    avl:avl_to_list(AVL, List),
    avl:avl_to_list(AVL2, List),
    !.
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


mppcg_notEqual(AVL, AVL2) :-
    avl:avl_to_list(AVL, List),
    avl:avl_to_list(AVL2, List2),
    mppcg_notEqual_(List, List2),
    !.
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
resolve(X-Y, Couple) :- resolve_couple(X-Y, Couple), !.
resolve([Head | Tail], List) :- resolve_list([Head | Tail], List), !.
resolve(X, X).
resolve_interval((A, B), List) :-
    nonvar(A),
    nonvar(B),
    !,
    findall(X, between(A, B, X), List).
resolve_couple((X-Y)-[Z], Couple) :-
    !,
    appendValueToKey(X-Y, Z, Couple).
resolve_couple(X-Y, X-Y).
resolve_list([], []).
resolve_list([X-Y | Tail], Result) :-
    !,
    mppcg_expandEntries(X, Y, Expanded),
    resolve_list(Tail, NewTail),
    append(Expanded, NewTail, Result).
resolve_list(List, List).