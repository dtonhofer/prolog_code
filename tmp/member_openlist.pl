foo_pushpull(List,Value,What) :-
   once(member_openlist(foo(Cs),List,What)),
   atom_chars(Value,Cs).
   
bar_pushpull(List,Value,What) :-
   once(member_openlist(bar(Cs),List,What)),
   atom_chars(Value,Cs).


member_openlist(Element,List,appended(Fin)) :-            % In this case, we are appending, so make sure "found" has not been requested.
   var(List),                                             % We found the the "fin" of an open list (an unbound variable).
   List=[Element|Fin].                                    % Append by instantiating "fin" to a new listbox and succeed.

member_openlist(Element,List,appended(FinalFin)) :-       % In this case, we are appending, so make sure "found" has not been requested.
   var(List),                                             % We found the the "fin" of an open list (an unbound variable).
   List=[_|NewFin],                                       % Append by instantiating "fin" to a new listbox ...
   member_openlist(Element,NewFin,appended(FinalFin)).    % ... and perform recursive call

member_openlist(Element,List,What) :-
   nonvar(List),                                          % List is instantiated to something, presumably a listbox [_|_]
   assertion((List == [] ; List = [_|_])),                % List should be a listbox or an empty list!
   member_openlist_2(Element,List,What).                  % Handle various cases


member_openlist_2(Element,[Element|B],found) :-           % Unified at end of proper list. Cut to be deterministic.
   B==[],                                                 % We can't put [Element] in the head lest we unify an open end with []
   !.

member_openlist_2(Element,[Element|_],found).             % Unified anywhere except at the end of a proper list. Do not cut!

member_openlist_2(Element,[_|B],What) :-                  % Keep on looking
   member_openlist(Element,B,What).


:- begin_tests(member_openlist).

test("nothing in an empty list",fail) :-
   member_openlist(x,[],_).

test("not found in a closed list",fail) :-
   member_openlist(x,[a,b,c],_).

test("found in the middle of a closed list") :-
   once(member_openlist(x,[a,x,c],What)),
   assertion(What==found).

test("found as last element of a closed list") :-
   member_openlist(x,[a,b,x],What),
   assertion(What==found).

test("instantiate them all over a closed list") :-
   findall(
      X-What,
      member_openlist(X,[a,b,c],What),
      Events),
   assertion(Events == [a-found,b-found,c-found]).

test("found in the middle of an open list") :-
   List=[a,x,c|_],
   once(member_openlist(x,List,What)),
   assertion(What==found),
   assertion((List=[a,x,c|Fin],var(Fin))).

test("found as last element of an open list") :-
   List=[a,b,x|_],
   once(member_openlist(x,List,What)),
   assertion(What==found),
   assertion((List=[a,b,x|Fin],var(Fin))).

test("not found in an open list leads to append") :-
   List=[a,b,c|_],
   once(member_openlist(d,List,What)),
   assertion((List = [a,b,c,d|Fin1],What=appended(Fin2),Fin1==Fin2,var(Fin1))).

test("found as element then appended using limit/2 inside findall/2") :-
   List=[a,b,x|_],
   findall(List-What, limit(2,member_openlist(x,List,What)), All),
   % Note the list collected by findall is not physically the same list as "List"
   assertion(
      All = [[a,b,x|_]-found,
             [a,b,x,x|Fin2]-appended(Fin2)]).

test("force appending and then close list") :-
   List=[a,b,x|_],
   once(member_openlist(x,List,appended(_))),
   once(member_openlist(x,List,appended(_))),
   once(member_openlist(a,List,appended([]))),
   assertion(List==[a,b,x,x,x,a]).

:- end_tests(member_openlist).
