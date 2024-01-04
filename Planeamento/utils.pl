:-module(utils,[is_member/2,format_insideFloorPath/2,format_caminho2/2]).

is_member(Element, []):-false.
is_member(Element, [Element|_]):-!.
is_member(Element, Element):-!.
is_member(Element, [N|REST]) :-
    is_member(Element, REST).


format_insideFloorPath(Caminho2, JsonObjects) :-
    format_insideFloorPath(Caminho2, [], JsonObjects).

% Helper predicate to format each element of Caminho2
format_insideFloorPath([], Acc, Result):-reverse(Acc,Result).
format_insideFloorPath([cel(X, Y) | Rest], Acc, Result) :-
    OK = json([cel=[X, Y]]),
    format_insideFloorPath(Rest, [OK | Acc], Result).

format_caminho2(Caminho2,JsonObjects):-
    format_caminho2(Caminho2,[],JsonObjects).

% Helper predicate to format each element of Caminho2

format_caminho2(Caminho2, JsonObjects) :-
    format_caminho2(Caminho2, [], JsonObjects).

% Helper predicate to format each element of Caminho2
format_caminho2([], Acc, Result):-reverse(Acc,Result).
format_caminho2([elev(X, Y) | Rest], Acc, Result) :-
    OK = json([elev=[X, Y]]),
    format_caminho2(Rest, [OK | Acc], Result).

format_caminho2([cor(X, Y) | Rest], Acc, Result) :-
    OK = json([cor=[X, Y]]),
    format_caminho2(Rest, [OK | Acc], Result).
