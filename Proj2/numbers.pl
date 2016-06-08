% ======================================================================
% File:             numbers.pl
% Created:          2015-10-09
% Last modified:    2015-10-16
% Author:           Robert Holt
% SID:              388648
% Email:            rholt@student.unimelb.edu.au
% Subject:          COMP90048 - Declarative Programming
% Purpose:          Project 2 - Number Puzzles
% ======================================================================
%
% This Prolog program provides a predicate to solve a 2-dimensional
% number grid puzzle of the form:
%      |  C1  |  C2  |  C3
%   R1 |  X11 |  X12 |  X13
%   R2 |  X21 |  X22 |  X23
%   R3 |  X31 |  X32 |  X33
% In which the following constraints are enforced:
%    - The square may be 2x2, 3x3, 4x4, ... (up to 9x9 is supported)
%    - Xi,j is an integer in 1..9
%    - The diagonal elements are all the same
%    - The same number is not repeated in each row or column
%    - Each row/column header (R|C)i is either the sum or the
%      product of all elements in that row or column
%
% Use this program by invoking the main predicate puzzle_solution/1.
%
% An example of the programs's use:
% > Puzzle = [[0, 14, 10, 35], [14, _, _, _], [15, _, _, _,], [28, _, 1, _,]],
% > | puzzle_solution(Puzzle).
% > Puzzle = [[0, 14, 10, 35], [14, 7, 2, 1], [15, 3, 7, 5], [28, 4, 1, 7]] ;
% > false.
% ======================================================================

% Load the constraint programming library.
:- ensure_loaded(library(clpfd)).

% puzzle_solution(?Puzzle)
% The main puzzle solving predicate. Provided an (N+1) x (N+1) square list,
% this will resolve all possible answers for that N x N puzzle. This take the
% format of a list of lists (being a square matrix), where the first list is
% the column header row (conventionally beginning with 0), and the remaining 
% lists are the row header followed by that row, i.e.:
% Puzzle = [[0,  c1,  c2,  ..],
%           [r1, X1,  X2,  ..],
%           [r2, X10, X11, ..],
%           ..               ]]
% where c1,c2,..,r1,r2,.. are also integers representing (independently) either
% the sum or product of their respective column or row.
% Because of the constraints on the domain, the puzzle dimension N must be
% less than 10 (this will fail automatically unless you manage to find a 
% new integer between 1 and 9).
puzzle_solution(Puzzle) :-
    strip_and_flatten(Puzzle, Sqr, Vars),
    % ins/2 is a clpfd predicate to constrain the domain
    % of list Vars to the range 1..9 (in this case)
    Vars ins 1..9,
    diag_all_same(Sqr),
    check_sqr(Puzzle),
    % labeling/2 non-deterministically tries each value in Vars to
    % satisfy the other predicates. The ff flag tells it
    % to attempt to ground variables in order of smallest
    % domain size (so that failure is likely to occur sooner).
    % labeling/2 is key to solving the puzzle, as it iterates
    % through the solution space given by all the other constraints.
    labeling([ff],Vars).

% strip_and_flatten(?Puz, +Sqr, -Vars)
% Strip the row/col headers from the puzzle square to
% obtain the variables (also returns a flattened list to
% make the main predicate more concise).
strip_and_flatten(Puz, Sqr, Vars) :-
    strip_sqr(Puz, Sqr),
    flatten(Sqr, Vars).

% strip_sqr(?Sqr, ?Stripped)
% Strips the row and column headers off a puzzle square (into Stripped),
% using the helper predicate strip_sqr_rows to strip out
% the row headers (stripping the column headers simply takes
% the tail of the list).
strip_sqr([_|Rows], Stripped) :- strip_sqr_rows(Rows, Stripped).
strip_sqr_rows([],[]).
strip_sqr_rows([[_|R_tail]|Rows], Stripped) :-
    append([R_tail], Rem_stripped, Stripped),
    strip_sqr_rows(Rows, Rem_stripped).

% diag_all_same(?Sqr)
% Check that the diagonal of a square is all the same value
diag_all_same(Sqr) :-
    get_diag(Sqr, Diag),
    list_of_same(Diag).

% get_diag(?Sqr, ?Diag)
% Get the diagonal Diag of a square Sqr, i.e.
% [[X, _, _],
%  [_, Y, _],   ->  [X,Y,Z]
%  [_, _, Z]]
% Uses the helper predicate diag_index, which carries
% the index of the diagonal element down the square
get_diag(Sqr, Diag) :- diag_index(Sqr, Diag, 0).
diag_index([], [], _).
diag_index([Row|Rows], Diag, Index) :-
    nth0(Index, Row, X),
    Index1 is Index + 1,
    append([X], Rem_diag, Diag),
    diag_index(Rows, Rem_diag, Index1).

% list_of_same(?List)
% Check that a list is composed entirely of the same element
list_of_same([]).
list_of_same([_]).
list_of_same([X1,X2|Xs]) :-
    X1 = X2,
    list_of_same([X2|Xs]).

% check_sqr(+Sqr)
% Check that the rows in the square sum or multiply to their headers,
% and that the same is true of the columns (in the transpose).
check_sqr(Sqr) :-
    check_all_rows(Sqr),
    transpose(Sqr, Sqr_t),
    check_all_rows(Sqr_t).

% check_all_rows(+Rows)
% Check all rows for constraint satisfaction recursively.
% Uses a helper predicate so the column headers can be ignored.
check_all_rows([_|Rows]) :- rows_check(Rows).
rows_check([]).
rows_check([Row|Rows]) :-
    check_row(Row),
    rows_check(Rows).

% check_row(+Row)
% Ensure a row satisfies the constraint that all elements are unique,
% and that the header value is either their sum or product
check_row([Head|Vars]) :-
    % all_different/1 is a clpfd predicate constraining Vars to
    % each have a different value - not as intelligent as
    % all_distinct/1, but lighter weight
    all_different(Vars),
    (dsum(Head, Vars) ; dproduct(Head, Vars)).

% dsum(?Sum, ?Ls)
% Compute the declarative sum of a list, so that it can be solved
% algebraically.
dsum(0, []).
dsum(Sum, [L|Ls]) :-
    % #=/2 is a clpfd constraint preserving the variability of
    % terms. It allows + to express a constraint on variables
    % rather than be evaluated immediately
    Sum #= L + Rem_sum,
    dsum(Rem_sum, Ls).

% dproduct(?Prod, ?Ls)
% Compute the declarative product of a list for algebraic solving.
dproduct(1, []).
dproduct(Prod, [L|Ls]) :-
    Prod #= L * Rem_prod,
    dproduct(Rem_prod, Ls).
