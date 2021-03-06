/*  Zero-Clause BSD (0BSD) follows (https://opensource.org/licenses/0BSD)

    Permission to use, copy, modify, and/or distribute this software for
    any purpose with or without fee is hereby granted.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
    WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
    AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
    DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
    TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
    PERFORMANCE OF THIS SOFTWARE.
*/

/*
Homepage for this code:

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_and_charylist_type.md
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


