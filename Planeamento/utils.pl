:-module(utils, [is_member/2, format_floorPath/2, format_path2/2]).

% is_member/2
is_member(X, []) :- false.
is_member(X, [X|_]).
is_member(X, [N|REST]) :- is_member(X, REST).

% format_floorPath/2
format_floorPath(Path2, JsonObjects) :-
    format_floorPath(Path2, [], JsonObjects).    

format_floorPath([], Acc, Result) :- reverse(Acc, Result).
format_floorPath([cel(X,Y)|REST], Acc, Result) :-
    OK = json([cel=[X,Y]]),
    format_floorPath(REST, [OK|Acc], Result).

% format_path2/2
format_path2(Path2, JsonObjects) :-
    format_path2(Path2, [], JsonObjects).

format_path2([], Acc, Result) :- reverse(Acc, Result).
format_path2([elevador(X,Y)|REST], Acc, Result) :-
    OK = json([elevador=[X,Y]]),
    format_path2(REST, [OK|Acc], Result).

format_path2([corredor(X,Y)|REST], Acc, Result) :-
    OK = json([corredor=[X,Y]]),
    format_path2(REST, [OK|Acc], Result).
