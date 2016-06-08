% Robert Holt
% SID: 388648
% COMP90048 Project 2

% Load the clpfd library
:- ensure_loaded(library(clpfd)).

% Test if Diag is an ordered list of the diagonal elements of Sqr
% Uses the helper predicate diag_index/3
get_diag(Sqr, Diag) :- diag_index(Sqr, Diag, 0).
diag_index([], [], _).
diag_index([Row|Rows], Diag, Index) :-
    nth0(Index, Row, X),
    Index1 is Index + 1,
    append([X], Rem_diag, Diag),
    diag_index(Rows, Rem_diag, Index1).

% Check the diagonal of a square is all the same element
check_diag(Sqr) :-
    get_diag(Sqr, Diag),
    list_of_same(Diag).

% Test if a list is composed entirely of the same element
list_of_same([]).
list_of_same([_]).
list_of_same([X1,X2|Xs]) :-
    X1 = X2,
    list_of_same([X2|Xs]).

dsum([], 0).
dsum([L|Ls], Sum) :-
    Sum #= L + Rem_sum,
    dsum(Ls, Rem_sum).

dproduct([], 1).
dproduct([L|Ls], Prod) :-
    Prod #= L * Rem_prod,
    dproduct(Ls, Rem_prod).

get_vars(Sqr, Vars) :- 
    strip_sqr(Sqr, Stripped),
    flatten(Stripped, Vars).

tail([_|T],T).
strip_sqr_rows([],[]).
strip_sqr_rows([Row|Rows], Stripped) :-
    tail(Row, R_tail),
    append([R_tail], Rem_stripped, Stripped),
    strip_sqr_rows(Rows, Rem_stripped).

strip_sqr([_|Rows], Stripped) :-
    strip_sqr_rows(Rows, Stripped).

check_row([Head_val|Row_items]) :-
    all_different(Row_items),
    dsum(Row_items, Head_val)
;
    all_different(Row_items),
    dproduct(Row_items, Head_val).

check_all_rows([]).
check_all_rows([R1|Rows]) :-
    check_row(R1),
    check_all_rows(Rows).

check_square([Head|Rows]) :-
    get_vars([Head|Rows], Vars),
    Vars ins 1..9,
    check_diag([Head|Rows]).


%Puzzle #= [[0,14,10,35],[14,_,_,_],[15,_,_,_],[28,_,1,_]].

