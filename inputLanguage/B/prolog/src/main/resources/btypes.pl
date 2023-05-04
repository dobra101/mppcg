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
    mppcg_listSum/2,
    mppcg_listProduct/2,
    mppcg_minus/3,
    mppcg_succ/2,
    mppcg_pred/2,
    mppcg_generalConcat/3,
    mppcg_inverse/2
    ]).

:- use_module(library(avl)).
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
mppcg_member(X, Y) :-
    nonvar(Y),
    resolve(X, X1),
    resolve(Y, Y1),
    !,
    mppcg_member_(X1, Y1).

mppcg_member_(X, 'INTEGER') :-
    var(X) -> gen_int_between(-infinite, infinite, X); number(X).
mppcg_member_(X, 'NATURAL') :-
    var(X) -> gen_int_between(0, infinite, X); between(0, infinite, X).
mppcg_member_(Set, function(From, To)) :-
    nonvar(From),
    nonvar(To),
    mppcg_member_function(Set, From, To).
mppcg_member_(Element, [Head | Tail]) :-
    (Element = (X/Y), Head = (_/_)) ->
    (sort([Head | Tail], Sorted),
    mppcg_member_relation(X, Y, Sorted));
    mppcg_member_list(Element, [Head | Tail]).

mppcg_member_relation(_, _, []) :- fail.
mppcg_member_relation(X, Y, [(X/Y) | _]).
mppcg_member_relation(X, Y, [(A/_) | Tail]) :-
    number(X),
    number(A),
    A =< X,
    mppcg_member_relation(X, Y, Tail), !.
mppcg_member_relation(X, Y, [_ | Tail]) :-
    mppcg_member_relation(X, Y, Tail).

mppcg_member_function((X/Y), From, To) :-
    mppcg_member_(X, From),
    mppcg_member_(Y, To).
mppcg_member_function(Set, From, To) :-
    nonvar(Set),
    mppcg_relation_split_xy(Set, Xs, Ys),
    sort(Xs, XSorted),
    sort(Ys, YSorted),
    mppcg_subset(XSorted, From),
    mppcg_subset(YSorted, To),
    !.

mppcg_relation_split_xy([], [], []).
mppcg_relation_split_xy([(X/Y) | Tail], [X | NewX], [Y | NewY]) :-
    mppcg_relation_split_xy(Tail, NewX, NewY).

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

mppcg_sortIfList(X, S) :-
    nonvar(X),
    is_list(X),
    !,
    sort(X, S).
mppcg_sortIfList(X, X).

mppcg_subset(X, Y) :-
    resolve(X, X1),
    resolve(Y, Y1),
    mppcg_sortIfList(X1, X2),
    mppcg_sortIfList(Y1, Y2),
    (is_list(Y2) -> mppcg_subset_(X2, Y2); mppcg_subset_infiniteSet(Y2, X2)).

mppcg_subset_infiniteSet(_, []).
mppcg_subset_infiniteSet(InfiniteSet, [Head | Tail]) :-
    mppcg_member_(Head, InfiniteSet),
    mppcg_subset_infiniteSet(InfiniteSet, Tail).
mppcg_subset_([], []).
mppcg_subset_([Head | NewTail], [Head | Tail]) :-
    mppcg_subset_(NewTail, Tail).
mppcg_subset_(NewTail, [_ | Tail]) :-
    mppcg_subset_(NewTail, Tail).

mppcg_subsetStrict(A, B) :-
    nonvar(A),
    nonvar(B),
    resolve(A, A1),
    resolve(B, B1),
    A1 \= B1,
    mppcg_subset(A1, B1).

mppcg_setSubtraction(Left, Right, set(Result)) :-
    nonvar(Left),
    nonvar(Right),
    resolve(Left, L),
    resolve(Right, R),
    !,
    findall(X, (mppcg_member(X, L), \+ mppcg_member(X, R)), Res),
    sort(Res, Result).

mppcg_setIntersection(Left, Right, set(Intersection)) :-
    nonvar(Left),
    nonvar(Right),
    resolve(Left, L),
    resolve(Right, R),
    !,
    findall(X, (mppcg_member(X, L), mppcg_member(X, R)), Inter),
    sort(Inter, Intersection).

mppcg_setUnion(Left, Right, set(Union)) :-
    nonvar(Left),
    nonvar(Right),
    resolve(Left, Left1),
    resolve(Right, Right1),
    append(Left1, Right1, Appended),
    !,
    sort(Appended, Union).

mppcg_domain(set(Relation), set(Sorted)) :-
    mppcg_domain(Relation, Domain),
    sort(Domain, Sorted).
mppcg_domain([], []) :- !.
mppcg_domain([(X/_) | Tail], Sorted) :-
    mppcg_domain(Tail, NewTail),
    sort([X | NewTail], Sorted).

mppcg_range(set(Relation), set(Sorted)) :-
    mppcg_range(Relation, Range),
    sort(Range, Sorted).
mppcg_range([], []) :- !.
mppcg_range([(_/Y) | Tail], Sorted) :-
    mppcg_range(Tail, NewTail),
    sort([Y | NewTail], Sorted).

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
     mppcg_member(X, Domain),
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
    \+ mppcg_member(X, Domain),
    mppcg_domainSubtraction(Domain, Tail, NewTail),
    !.
mppcg_domainSubtraction(Domain, [_ | Tail], NewTail) :-
    mppcg_domainSubtraction(Domain, Tail, NewTail),
    !.

mppcg_image(Relation, Set, set(Sorted)) :-
    resolve(Set, S),
    mppcg_image_(Relation, S, Image),
    sort(Image, Sorted),
    !.
mppcg_image_(_, [], []) :- !.
% image of set
mppcg_image_(Relation, [X | Rest], Image) :-
    mppcg_image_X(Relation, X, Ys),
    mppcg_image_(Relation, Rest, OtherTail),
    append(Ys, OtherTail, Image),
    !.
mppcg_image_(Relation, [_ | Rest], NewTail) :-
    mppcg_image_(Relation, Rest, NewTail).


mppcg_image_X([], _, []).
mppcg_image_X([(X/Y) | Tail], X, [Y | NewTail]) :-
    !,
    mppcg_image_X(Tail, X, NewTail).
mppcg_image_X([_ | Tail], X, NewTail) :-
    mppcg_image_X(Tail, X, NewTail).


mppcg_override(Overridden, Set, Result) :-
    resolve(Overridden, O),
    resolve(Set, S),
    mppcg_domain(S, Domain),
    mppcg_domainSubtraction(Domain, O, Tail),
    append(S, Tail, R),
    sort(R, Result).

mppcg_rangeRestriction(Relation, set(Set), set(Result)) :-
    mppcg_rangeRestriction(Relation, Set, Result),
    !.
mppcg_rangeRestriction(set(Relation), set(Set), set(Result)) :-
    mppcg_rangeRestriction(Relation, Set, Result),
    !.
mppcg_rangeRestriction([], _, []) :- !.
mppcg_rangeRestriction([(X/Y) | Tail], Range, [(X/Y) | NewTail]) :-
    mppcg_member(Y, Range),
    mppcg_rangeRestriction(Tail, Range, NewTail),
    !.
mppcg_rangeRestriction([_ | Tail], Range, NewTail) :-
    mppcg_rangeRestriction(Tail, Range, NewTail),
    !.

mppcg_rangeSubtraction(Relation, Set, set(Result)) :-
    resolve(Relation, R),
    resolve(Set, S),
    mppcg_rangeSubtraction_(R, S, Result).
mppcg_rangeSubtraction_(Relation, set(Set), set(Result)) :-
    mppcg_rangeSubtraction_(Relation, Set, Result),
    !.
mppcg_rangeSubtraction_([], _, []) :- !.
mppcg_rangeSubtraction_([(X/Y) | Tail], Range, [(X/Y) | NewTail]) :-
    \+ mppcg_member(Y, Range),
    mppcg_rangeSubtraction_(Tail, Range, NewTail),
    !.
mppcg_rangeSubtraction_([_ | Tail], Range, NewTail) :-
    mppcg_rangeSubtraction_(Tail, Range, NewTail),
    !.

mppcg_powerSet(X, PowerSet) :-
    resolve(X, X1),
    mppcg_powerSet_(X1, PowerSet).
mppcg_powerSet_(Set, set(Sorted)) :-
    findall(set(Subset), (mppcg_subset(Sub, Set), sort(Sub, Subset)), Powerset),
    sort(Powerset, Sorted),
    !.

mppcg_powerSet1(X, PowerSet) :-
    resolve(X, X1),
    mppcg_powerSet1_(X1, PowerSet).
mppcg_powerSet1_(Set, set(Sorted)) :-
    findall(set(Subset), (mppcg_subset(Sub, Set), sort(Sub, Subset), Subset \= []), Powerset),
    sort(Powerset, Sorted),
    !.

mppcg_max(X, Max) :-
    resolve(X, X1),
    mppcg_max_(X1, unset, Max),
    !.
mppcg_max_([], unset, unset) :- fail, !.
mppcg_max_([Head | Tail], unset, Max) :-
    mppcg_max_(Tail, Head, Max),
    !.
mppcg_max_([], Acc, Acc) :- !.
mppcg_max_([Head | Tail], Acc, Max) :-
    Head > Acc,
    mppcg_max_(Tail, Head, Max),
    !.
mppcg_max_([_ | Tail], Acc, Max) :-
    mppcg_max_(Tail, Acc, Max),
    !.

mppcg_min(X, Min) :-
    resolve(X, X1),
    mppcg_min_(X1, unset, Min),
    !.
mppcg_min_([], unset, unset) :- fail, !.
mppcg_min_([Head | Tail], unset, Min) :-
    mppcg_min_(Tail, Head, Min),
    !.
mppcg_min_([], Acc, Acc) :- !.
mppcg_min_([Head | Tail], Acc, Min) :-
    Head < Acc,
    mppcg_min_(Tail, Head, Min),
    !.
mppcg_min_([_ | Tail], Acc, Min) :-
    mppcg_min_(Tail, Acc, Min),
    !.

mppcg_card(X, Card) :-
    resolve(X, X1),
    mppcg_card_(X1, Card).
mppcg_card_(List, Length) :-
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

mppcg_sequenceRestrictTail(Sequence, 0, Sequence) :- !.
mppcg_sequenceRestrictTail([_ | Tail], N, Restricted) :-
    N1 is N - 1,
    mppcg_sequenceRestrictTail(Tail, N1, Restricted).

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

mppcg_equal(X, Y) :-
    resolve(X, X1),
    resolve(Y, Y1),
    mppcg_equal_(X1, Y1), !.
mppcg_equal_(X, X) :- !.
mppcg_equal_(X, Y) :- number(X), number(Y), !, X =< Y, X >= Y. % e.g. X = 1.0, Y = 1
mppcg_equal_(List1, List2) :-
    is_list(List1),
    is_list(List2),
    sort(List1, S1),
    sort(List2, S1).

mppcg_notEqual(X, Y) :-
    resolve(X, X1),
    resolve(Y, Y1),
    mppcg_notEqual_(X1, Y1), !.
mppcg_notEqual_(X, X) :- !, fail.
mppcg_notEqual_(X, Y) :- number(X), number(Y), !, (X < Y; X > Y).
mppcg_notEqual_(X, Y) :- X \= Y, !.
mppcg_notEqual_(SetOrInterval1, SetOrInterval2) :-
    (mppcg_member(X, SetOrInterval1), \+ mppcg_member(X, SetOrInterval2));
    (mppcg_member(X, SetOrInterval2), \+ mppcg_member(X, SetOrInterval1)), !.

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
    (is_list(Parameters) ->
    mppcg_callFunction_multiParameter(Parameters, Expression, Res);
    mppcg_callFunction_singleParameter(Parameters, Expression, Res)),
    flatten(Res, Flat),
    !,
    (length(Flat, 1) -> Flat = [Result]; Result = Flat).

mppcg_callFunction_parameter([], _) :- fail.
mppcg_callFunction_parameter([X | _], X) :- !.
mppcg_callFunction_parameter([_ | Tail], X) :-
    mppcg_callFunction_parameter(Tail, X).

mppcg_callFunction_multiParameter(_, [], []).
mppcg_callFunction_multiParameter(Parameters, [(X/Y) | Tail], [Y | YTail]) :-
    !,
    mppcg_callFunction_parameter(Parameters, X),
    mppcg_callFunction_multiParameter(Parameters, Tail, YTail).
mppcg_callFunction_multiParameter(Parameters, [_ | Tail], YTail) :-
    mppcg_callFunction_multiParameter(Parameters, Tail, YTail).

mppcg_callFunction_singleParameter(_, [], []).
mppcg_callFunction_singleParameter(X, [(X/Y) | Tail], [Y | YTail]) :-
    !,
    mppcg_callFunction_singleParameter(X, Tail, YTail).
mppcg_callFunction_singleParameter(X, [_ | Tail], YTail) :-
    mppcg_callFunction_singleParameter(X, Tail, YTail).



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
    resolve(X, X1),
    resolve(Y, Y1),
    mppcg_minus_(X1, Y1, Result).
mppcg_minus_(List1, List2, Result) :-
    findall(X, (mppcg_member(X, List1), \+ mppcg_member(X, List2)), Result).

resolve(X, X) :- var(X).
resolve('NAT', List) :- maxInt(Max), resolve_interval((0, Max), List), !.
resolve('NAT1', List) :- maxInt(Max), resolve_interval((1, Max), List), !.
resolve('INT', List) :- minInt(Min), maxInt(Max), resolve_interval((Min, Max), List), !.
resolve('BOOL', [true, false]) :- !.
resolve(set(List), List) :- !.
resolve((A, B), List) :- resolve_interval((A, B), List), !.
resolve(function(From, To), function(F, T)) :-
    resolve(From, F),
    resolve(To, T).
resolve(X, X).
resolve_interval((A, B), List) :-
    nonvar(A),
    nonvar(B),
    !,
    findall(X, between(A, B, X), List).

mppcg_pred(X, Pred) :- Pred is X - 1.
mppcg_succ(X, Succ) :- Succ is X + 1.

mppcg_generalConcat(First, Set, Concat) :-
    findall(M, mppcg_member(M, Set), List),
    append([First], List, All),
    sort(All, Sorted),
    mppcg_generalConcat(Sorted, Concat).
mppcg_generalConcat([], []).
mppcg_generalConcat([(_/X) | Tail], Concat) :-
    mppcg_generalConcat(Tail, Rest),
    append(X, Rest, Concat).

mppcg_inverse(Set, Result) :-
    resolve(Set, S), !,
    mppcg_inverse_(S, Result).
mppcg_inverse_([], []).
mppcg_inverse_([(X/Y) | Tail], [(Y/X) | NewTail]) :-
    mppcg_inverse_(Tail, NewTail).