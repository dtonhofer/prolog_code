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

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_length.md
*/

:- use_module(library('onepointfour_basics/stringy_length.pl')).

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
   stringy_length("hello",5,hard).

test("reject length of string",fail) :-
   stringy_length("hello",4,hard).

test("reject negative length (default)",fail) :-
   stringy_length("hello",-1).

test("reject negative length if tuned to 'soft'",fail) :-
   stringy_length("hello",-1,soft).

test("exception on negative length if tuned to 'hard'",error(check(domain,_,_,_))) :-
   stringy_length("hello",-1,hard).

test("exception on unbound arg 1",error(check(instantiation,_,_,_))) :-
   stringy_length(_,_).

test("exception on non-stringy arg 1",error(check(type,_,_,_))) :-
   stringy_length(f(x),_).

test("exception on non-number arg 2",error(check(type,_,_,_))) :-
   stringy_length("hello",foo).

test("exception on non-integer arg 2",error(check(type,_,_,_))) :-
   stringy_length("hello",1.22).

:- end_tests(stringy_length).

