% query for doc_server(9000).
% then nagivate to http://localhost:9000/pldoc

:- module(pldoc_demo, []).
/** <module> Demo of pldoc functionality
 */

:- multifile  quiz_predicate/1.

%!   quiz_predicate(+X:list) is det
%
%   predicate to demo pldoc functionality
quiz_predicate(X) :-
    writeln(X).
