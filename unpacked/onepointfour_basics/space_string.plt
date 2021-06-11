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

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_space_string.md
*/

:- use_module(library('onepointfour_basics/space_string.pl')).

:- begin_tests(space_string).

test("generate a short string", true(Str == "     ")) :-
   space_string(5,Str).

test("generate a long string", true(Str == "               ")) :-
   space_string(15,Str).

test("verify that a string has a certain length") :-
   space_string(5,"     ").

test("obtain the length of a string",true(L == 5)) :-
   space_string(L,"     ").

test("obtain the length of an atom",true(L == 5)) :-
   space_string(L,'     ').

test("fail verifiying a non-space string",fail) :-
   space_string(_,"Hello World").

test("fail verifying length of string",fail) :-
   space_string(50,"     ").

test("fail obtaining length of non-space string",fail) :-
   space_string(_,"XXX").

test("throw on non-string",error(check(type,_,_,_))) :-
   space_string(_,[]).

test("fail on non-string if smooth",fail) :-
   space_string_smooth(_,[]).

test("throw on non-string #2",error(_,_)) :-
   space_string(_,[],throw).

test("fail on negative length",fail) :-
   space_string(-1,_).

test("throw on non-integer",error(check(type,_,_,_))) :-
   space_string(foo,_).

test("fail on non-integer if smooth",fail) :-
   space_string_smooth(foo,_).

test("fail on negative length by default",fail) :-
   space_string(-1,_).

test("throw on negative length on request",error(check(domain,_,_,_))) :-
   space_string(-1,_,throw).

test("generate pairs", true(Bag == [0-"",1-" ",2-"  ",3-"   ",4-"    "])) :-
   length(Bag,5),
   bagof(L-S,limit(5,space_string(L,S)),Bag).

test("generate multiple strings") :-
   forall(
      between(0,2000,Len),
      (
         space_string(Len,Spaces),
         string_length(Spaces,Len)
      )
   ).

:- end_tests(space_string).

