:- module(adultschildren, [
              go/0
          ]).
/** <module> Solve the adults-and-children river problem
 *
 * 3 adults, 2 children, and a dog must cross a river, using
 * a small boat.
 *
 * Unfortunately, the boat is only big enough for a single adult,
 * or 2 children, or a child and a dog.
 *
 * Anyone except the dog can paddle the boat.
 *
 * assume everyone starts on the left bank
 *
 * This is a teaching example, so I've heavily commented it,
 * undoubtedly more than would be good practice for 'real' code
 *
 */

%!  init(-Conditions:state) is det
%
%   pass out the initial conditions
init(left(3,2,1)).

%!    go is nondet
%
%   run the problem. On backtracking, give another solution.
%
go :-
    init(Init),
    crossing_plan([Init], Plan),
    write_plan(Init, Plan).


%!  crossing_plan(+States:list, -Plan:list) is nondet
%
%   true if States is a list of states the system's been in, most recent
%   on left, and Plan is a list of steps to reach the goal from here
%
%   @arg States a list of left(A,C,D) meaning 'boat on left bank, A
%   adults, C children, D dogs on left bank) and right(A,C,D) where the
%   numbers are *|still for the left bank|* but boat is on right
%
%   @arg Plan is a list of moves lr(A,C,D) and rl(A,C,D) crossings with
%   this number of adults, children, and dogs.

% nobody on left bank, we're done
crossing_plan([right(0,0,0) | _], []).
% boat on left bank, send some folks across
crossing_plan([left(A,C,D) | Past], [Move | Plan]) :-
    % not that many legal combos, lets just list them
    member(Move, [lr(1,0,0),
                  lr(0,1,0),
                  lr(0,2,0),
                  lr(0,1,1)]),
    Move = lr(BoatA, BoatC, BoatD),   % destructure
    BoatA =< A,   % have we got enough folks to do this?
    BoatC =< C,
    BoatD =< D,
    % figure out what the new state is. Note that we're also
    % doing some sanity checks on the move
    new_state(left(A,C,D), Move, NewState),
    \+ member(NewState, [left(A,C,D) | Past]), % make progress!
    crossing_plan([NewState, left(A,C,D) | Past], Plan).
% and now we do the same for rl journey, remembering numbers
% are for the left bank
crossing_plan([right(A,C,D) | Past], [Move | Plan]) :-
    % rearrange the possibilities so we use the most likely first
    % also, never move an adult or the dog back, they can't help
    % others cross
    % (search doesn't mean domain knowledge is useless)
    member(Move, [rl(0,2,0),
                  rl(0,1,0)]),
    Move = rl(BoatA, BoatC, BoatD),   % destructure
    init(left(TotalA, TotalC, TotalD)), % need # on right bank
    BoatA =< TotalA - A,   % have we got enough folks to do this?
    BoatC =< TotalC - C,
    BoatD =< TotalD - D,
    % figure out what the new state is.
    new_state(right(A,C,D), Move, NewState),
    % \+ is 'not provable'
    % we must not be able to prove we were here before
    \+ member(NewState, [right(A,C,D) | Past]), % make progress!
    crossing_plan([NewState, right(A,C,D) | Past], Plan).

% little helper to figure out new state when you move the boat.
% It might be reasonable to do sanity checks here.
new_state(left(A,C,D), lr(BoatA, BoatC, BoatD), right(NewA, NewC, NewD)) :-
    NewA is A - BoatA,
    NewC is C - BoatC,
    NewD is D - BoatD.
new_state(right(A,C,D), rl(BoatA, BoatC, BoatD), left(NewA, NewC, NewD)) :-
    NewA is A + BoatA,
    NewC is C + BoatC,
    NewD is D + BoatD.

%!  write_plan(+State:state, +Plan:list) is det
%
%   Write out the plan
%
write_plan(right(0,0,0), []) :-
    writeln('done.').   % this atom must be single quoted because of period
write_plan(left(A,C,D), [lr(BoatA, BoatC, BoatD) | Rest]) :-
    new_state(left(A,C,D),
              lr(BoatA, BoatC, BoatD),
              NewState),
    format('(~w,~w,~w)~n', [A,C,D]),
    format('          ==(~w,~w,~w)==>~n', [BoatA, BoatC, BoatD]),
    write_plan(NewState, Rest).
write_plan(right(A,C,D), [rl(BoatA, BoatC, BoatD) | Rest]) :-
    new_state(right(A,C,D),
              rl(BoatA, BoatC, BoatD),
              NewState),
    init(left(TotalA, TotalC, TotalD)),
    RightA is TotalA - A,
    RightC is TotalC - C,
    RightD is TotalD - D,
    format('                                (~w,~w,~w)~n',
           [RightA, RightC, RightD]),
    format('           <==(~w,~w,~w)===~n', [BoatA, BoatC, BoatD]),
    write_plan(NewState, Rest).
