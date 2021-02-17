:- use_module(library('onepointfour_strings/texty_typetests.pl')).

:- begin_tests(texty_typetests).

test(compound,fail)       :- texty(f(1),_).
test(bad_list_1,fail)     :- texty([a,b,4],_).
test(bad_list_2,fail)     :- texty([4,5,x],_).
test(bad_list_3,fail)     :- texty([a,b,f(x)],_).
test(bad_list_4,fail)     :- texty([4,5,-1],_).
test(bad_list_5,fail)     :- texty([a,b,c,dd],_).
test(bad_list_6,fail)     :- texty([a,b,c,''],_).
test(bad_list_7,fail)     :- texty([a,[],4,d],_).
test(bad_list_8,fail)     :- texty([a,_,4,d],_).
test(bad_list_9,fail)     :- texty([1,_,_,d],_).
test(bad_openlist_1,fail) :- texty([0,c|_],_).
test(bad_nonlist_1,fail)  :- texty([0,1|foo],_).
test(dict,fail)           :- texty(foo{1:x},_).   

test(var) :- texty(_,R),assertion(R==var).
   
test(emptylist) :- texty([],R),assertion(R==emptylist).

test(atom_0)    :- texty('',R),assertion(R==atom(0)).
test(atom_1)    :- texty('a',R),assertion(R==atom(1)).
test(atom_2)    :- texty('aa',R),assertion(R==atom(2)).

test(string_0)  :- texty("",R),assertion(R==string(0)).
test(string_1)  :- texty("a",R),assertion(R==string(1)).
test(string_2)  :- texty("aa",R),assertion(R==string(2)).

test(number_int)    :- texty(100,R),assertion(R==number).
test(number_float)  :- texty(3.1416,R),assertion(R==number).
test(number_rrat)   :- texty(4r5,R),assertion(R==number).

test(charlist)  :- texty([a,b,c],R),assertion(R==charlist(3)).
test(codelist)  :- atom_codes(abc,Codes),texty(Codes,R),assertion(R==codelist(3)).

test(varlist_1)  :- texty([_],R),assertion(R==varlist(1)).
test(varlist_2)  :- texty([_,_],R),assertion(R==varlist(2)).
test(varlist_3)  :- texty([_,_,_],R),assertion(R==varlist(3)).

test(charylist_2)  :- texty([a,b,_],R),assertion(R==charylist(2,1,3)).
test(charylist_1)  :- texty([_,_,c],R),assertion(R==charylist(1,2,3)).

test(codeylist)  :- texty([40,41,_],R),assertion(R==codeylist(2,1,3)).
test(codeylist)  :- texty([_,_,43],R),assertion(R==codeylist(1,2,3)).

test(openvarlist_1)  :- texty([_|_],R),assertion(R==openvarlist(1)).
test(openvarlist_2)  :- texty([_,_|_],R),assertion(R==openvarlist(2)).
test(openvarlist_3)  :- texty([_,_,_|_],R),assertion(R==openvarlist(3)).

test(opencharylist_1)  :- texty([a|_],R),assertion(R==opencharylist(1,0,1)).
test(opencharylist_2)  :- texty([a,b|_],R),assertion(R==opencharylist(2,0,2)).
test(opencharylist_3)  :- texty([a,b,c|_],R),assertion(R==opencharylist(3,0,3)).
test(opencharylist_4)  :- texty([a,_,c|_],R),assertion(R==opencharylist(2,1,3)).
test(opencharylist_5)  :- texty([_,_,c|_],R),assertion(R==opencharylist(1,2,3)).

test(opencodeylist_1)  :- texty([40|_],R),assertion(R==opencodeylist(1,0,1)).
test(opencodeylist_2)  :- texty([40,41|_],R),assertion(R==opencodeylist(2,0,2)).
test(opencodeylist_3)  :- texty([40,41,42|_],R),assertion(R==opencodeylist(3,0,3)).
test(opencodeylist_4)  :- texty([40,_,42|_],R),assertion(R==opencodeylist(2,1,3)).
test(opencodeylist_5)  :- texty([_,_,42|_],R),assertion(R==opencodeylist(1,2,3)).

:- end_tests(texty_typetests).
