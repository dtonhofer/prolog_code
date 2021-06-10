/*  MIT License Follows (https://opensource.org/licenses/MIT)

    Copyright 2021 David Tonhofer <ronerycoder@gluino.name>

    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files
    (the "Software"), to deal in the Software without restriction,
    including without limitation the rights to use, copy, modify, merge,
    publish, distribute, sublicense, and/or sell copies of the Software,
    and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

:- use_module(library('onepointfour_basics/stringy_and_charylist_type.pl')).

:- begin_tests(charylist_type).

test("empty charys") :-
   charylist_type([],T),
   assertion(T==empty).

test("charys made of chars") :-
   charylist_type([a,b,c,d],T),
   assertion(T==chars(4)).

test("charys made of chars and vars") :-
   charylist_type([a,_,_,d,e],T),
   assertion(T==chars_vars(3,2)).

test("charys made of codes") :-
   charylist_type([0'a,0'b,0'c,0'd],T),
   assertion(T==codes(4)).

test("charys made of codes and vars") :-
   charylist_type([0'a,_,_,0'd,0'e],T),
   assertion(T==codes_vars(3,2)).

test("charys made of vars") :-
   charylist_type([_,_,_,_],T),
   assertion(T==vars(4)).

test("charys that is a var") :-
   charylist_type(_,T),
   assertion(T==var).

test("charys that is a mix of code and chars fails",fail) :-
   charylist_type([a,b,c,0'a,0'b,0'c],T),
   assertion(T==var).

test("charys that is an open list fails",fail) :-
   charylist_type([a,b,c|_],_).

test("charys that is some compound term fails",fail) :-
   charylist_type(f(x),_).

test("verify charys made of chars") :-
   charylist_type([a,b,c,d],chars(N)),
   assertion(N==4).

test("verify charys made of chars") :-
   charylist_type([a,b,c,d],chars(4)).

test("verify charys made of chars and vars") :-
   charylist_type([a,b,_,c,d,_],chars_vars(N,V)),
   assertion(N==4),
   assertion(V==2).

:- end_tests(charylist_type).

% -----------------------------------------------------------------------------

:- begin_tests(stringy_type_with_length).

test("empty string") :-
   stringy_type_with_length("",T),
   assertion(T==string(0)).

test("empty atom") :-
   stringy_type_with_length('',T),
   assertion(T==atom(0)).

test("var") :-
   stringy_type_with_length(_,T),
   assertion(T==var).

test("nonempty string") :-
   stringy_type_with_length("foo",T),
   assertion(T==string(3)).

test("nonempty atom") :-
   stringy_type_with_length(foo,T),
   assertion(T==atom(3)).

test("test of non-stringy stuff #1",fail) :-
   stringy_type_with_length(f(g(x)),_).

test("test of non-string stuff #2",fail) :-
   stringy_type_with_length([],_).

test("verify string") :-
   stringy_type_with_length("foo",string(3)).

test("fail to verify string",fail) :-
   stringy_type_with_length(foo,string(3)).

test("fail to verify atom",fail) :-
   stringy_type_with_length("foo",atom(3)).

test("fail to verify string based on bad length #1",fail) :-
   stringy_type_with_length("hello world",string(3)).

test("fail to verify string based on bad length #2",fail) :-
   stringy_type_with_length("hello world",string(-1)).

:- end_tests(stringy_type_with_length).

% -----------------------------------------------------------------------------

:- begin_tests(stringy_type).

test("empty string") :-
   stringy_type("",T),
   assertion(T==string).

test("empty atom") :-
   stringy_type('',T),
   assertion(T==atom).

test("var") :-
   stringy_type(_,T),
   assertion(T==var).

test("nonempty string") :-
   stringy_type("foo",T),
   assertion(T==string).

test("nonempty atom") :-
   stringy_type(foo,T),
   assertion(T==atom).

test("test of non-stringy stuff #1",fail) :-
   stringy_type(f(g(x)),_).

test("test of non-string stuff #2",fail) :-
   stringy_type([],_).

test("verify string") :-
   stringy_type("foo",string).

test("fail to verify string",fail) :-
   stringy_type(foo,string).

test("fail to verify atom",fail) :-
   stringy_type("foo",atom).

:- end_tests(stringy_type).

% -----------------------------------------------------------------------------

:- begin_tests(stringy_length).

test("find length of string") :- 
   stringy_length("hello",L),
   assertion(L==5).

test("find length of atom") :- 
   stringy_length(hello,L),
   assertion(L==5).

test("find length of empty string") :- 
   stringy_length("",L),
   assertion(L==0).

test("find length of empty atom") :- 
   stringy_length('',L),
   assertion(L==0).

test("accept length of string") :- 
   stringy_length("hello",5).

test("reject length of string",fail) :- 
   stringy_length("hello",4).

test("accept length of string") :- 
   stringy_length("hello",5,throw).

test("reject length of string",fail) :- 
   stringy_length("hello",4,throw).

test("reject negative length",fail) :- 
   stringy_length("hello",-1).

test("throw on negative length if tuned to 'hard'",error(check(domain,_,_,_))) :- 
   stringy_length("hello",-1,throw).

test("exception on unbound arg 1",error(check(too_little_instantiation,_,_,_))) :-
   stringy_length(_,_).

test("exception on non-stringy arg 1",error(check(type,_,_,_))) :-
   stringy_length(f(x),_).

test("exception on non-number arg 2",error(check(type,_,_,_))) :-
   stringy_length("hello",foo).

test("exception on non-integer arg 2",error(check(type,_,_,_))) :-
   stringy_length("hello",1.22).

test("exception on negative length if strict",error(check(domain,_,_,_))) :-
   stringy_length("hello",-1,throw).

:- end_tests(stringy_length).

