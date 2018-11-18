% Rules:
%   - A board has eight queens.
%   - Each queen has a row from 1 - 8 and a column from 1 - 8.
%   - No two queens can share the same row.
%   - No two queens can share the same column.
%   - No two queens can share the same diagonal (southwest to northeast).
%   - No two queens can share the same diagonal (northwest to southeast).
%
% Test:
%   eight_queens([(1, A), (2, B), (3, C), (4, D), (5, E), (6, F), (7, G), (8, H)]).
eight_queens(Board) :-
  length(Board, 8),
  valid_board(Board),

  rows(Board, Rows),
  cols(Board, Cols),
  diags1(Board, Diags1),
  diags2(Board, Diags2),

  fd_all_different(Rows),
  fd_all_different(Cols),
  fd_all_different(Diags1),
  fd_all_different(Diags2).

% Test:
%   eight_queens_opt([(1, A), (2, B), (3, C), (4, D), (5, E), (6, F), (7, G), (8, H)]).
eight_queens_opt(Board) :-
  Board = [(1, _), (2, _), (3, _), (4, _), (5, _), (6, _), (7, _), (8, _)],
  valid_board_opt(Board),

  cols(Board, Cols),
  diags1(Board, Diags1),
  diags2(Board, Diags2),

  fd_all_different(Cols),
  fd_all_different(Diags1),
  fd_all_different(Diags2).

% The predicate `member` does just what you think; it tests for membership. A
% queen is valid if both the row and column are integers from 1 - 8.
valid_queen((Row, Col)) :-
  Range = [1, 2, 3, 4, 5, 6, 7, 8],
  member(Row, Range),
  member(Col, Range).

valid_board([]).
valid_board([H|T]) :-
  valid_queen(H),
  valid_board(T).

valid_queen_opt((_, Col)) :-
  member(Col, [1, 2, 3, 4, 5, 6, 7, 8]).

valid_board_opt([]).
valid_board_opt([H|T]) :-
  valid_queen_opt(H),
  valid_board_opt(T).


% What are the rows, columns, and diagonals? `rows` for an empty list is an
% empty list, and `rows(Queens, Rows)` is `Rows` if the `Row` from the tail of
% `Queens` is the tail of `Rows`.
rows([], []).
rows([(Row, _)|QT], [Row|RT]) :-
  rows(QT, RT);

cols([], []).
cols([(_, Col)|QT], [Col|CT]) :-
  cols(QT, CT).

diags1([], []).
diags1([(Row, Col)|QT], [Diag|DT]) :-
  Diag is Col - Row,
  diags1(QT, DT).

diags2([], []).
diags2([(Row, Col)|QT], [Diag|DT]) :-
  Diag is Col + Row,
  diags2(QT, DT).
