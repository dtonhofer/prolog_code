:- use_module('path_list.pl').

:- begin_tests(is_path_list).

test(1) :- 
   is_path_list([],Type,Ground),
   assertion(var(Type)),
   assertion(Ground==true).

test(2) :- 
   is_path_list([frag(hello)],Type,Ground),
   assertion(Type==atom),
   assertion(Ground==true).

test(3) :- 
   is_path_list([frag("hello")],Type,Ground),
   assertion(Type==string),
   assertion(Ground==true).

test(4) :- 
   is_path_list([frag("hello"),slashes(1),frag(world)],Type,Ground),
   assertion(Type==any),
   assertion(Ground==true).
   
test(5) :-
    is_path_list([frag("hello"),slashes(1),frag(foo)],Type,Ground),
    assertion(Type==any),
    assertion(Ground==true).

test(5) :-
    is_path_list([_,slashes(1),frag(foo)],Type,Ground),
    assertion(Type==atom),
    assertion(Ground==false).

test(6) :-
    is_path_list([_,_,frag(foo)],Type,Ground),
    assertion(Type==atom),
    assertion(Ground==false).

test(7) :-    
   is_path_list([slashes(1),frag(hello),slashes(1),frag(world),slashes(2)],atom,true).    
    
test(8,fail) :-    
   is_path_list([slashes(1),frag(hello),slashes(0),frag(world),slashes(2)],atom,true).    

test(9,fail) :-    
   is_path_list([slashes(1),frag(hello),frag(world),slashes(2)],atom,true).    
   
:- end_tests(is_path_list).



:- begin_tests(path_text_list).

expected(PathText,PathList) :-
   path_text_list(PathText,PathListOut),
   assertion(PathListOut==PathList),
   path_text_list(PathTextOut,PathList),
   assertion(PathTextOut==PathText).

test(parse_1,[nondet]) :- 
   expected("",[]).
   
test(parse_2,[nondet]) :- 
   expected("/",[slashes(1)]).
   
test(parse_3,[nondet]) :- 
   expected("////",[slashes(4)]).

test(parse_4_string,[nondet]) :- 
   expected("/a/b/c",[slashes(1),frag("a"),slashes(1),frag("b"),slashes(1),frag("c")]).

test(parse_4_atom,[nondet]) :- 
   expected('/a/b/c',[slashes(1),frag(a),slashes(1),frag(b),slashes(1),frag(c)]).
   
test(parse_5_string,[nondet]) :- 
   expected("/a/b/c",[slashes(1),frag("a"),slashes(1),frag("b"),slashes(1),frag("c")]).

test(parse_5_atom,[nondet]) :- 
   expected('/a/b/c',[slashes(1),frag(a),slashes(1),frag(b),slashes(1),frag(c)]).
   
test(parse_6_string,[nondet]) :- 
   expected("foo/bar/baz",[frag("foo"),slashes(1),frag("bar"),slashes(1),frag("baz")]).

test(parse_6_atom,[nondet]) :- 
   expected('foo/bar/baz',[frag(foo),slashes(1),frag(bar),slashes(1),frag(baz)]).
   
test(parse_7_string,[nondet]) :-    
   expected("/foo/bar/baz//",[slashes(1),frag("foo"),slashes(1),frag("bar"),slashes(1),frag("baz"),slashes(2)]).

test(parse_7_atom,[nondet]) :-    
   expected('/foo/bar/baz//',[slashes(1),frag(foo),slashes(1),frag(bar),slashes(1),frag(baz),slashes(2)]).

test(parse_8_string,fail) :- 
   path_text_list("/a/b/c",[frag("a"),slashes(1),frag("b"),slashes(1),frag("c")]).

test(parse_9_string,fail) :- 
   path_text_list("/a/b/c",[frag("a"),slashes(0),frag("b"),slashes(1),frag("c")]).

test(parse_10_string) :- 
   path_text_list("a/b//c",[frag("a"),slashes(X),frag(Y),slashes(Z),frag("c")]),
   assertion(X == 1),
   assertion(Y == "b"),
   assertion(Z == 2).
   
:- end_tests(path_text_list).
