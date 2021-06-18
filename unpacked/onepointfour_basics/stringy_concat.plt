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

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_concat.md
*/

:- use_module(library('onepointfour_basics/stringy_concat.pl')).

:- begin_tests(stringy_concat).

test("generate empty atom from empty list") :-
   stringy_concat([],S,atom),
   assertion(S == '').

test("generate empty string from empty list") :-
   stringy_concat([],S,string),
   assertion(S == "").

test("accept empty string, yielding type as side-dish") :-
   stringy_concat([],"",Type),
   assertion(Type == string).

test("accept empty atom, yielding type as side-dish") :-
   stringy_concat([],'',Type),
   assertion(Type == atom).

test("simple concat, accept, #1") :-
   stringy_concat([foo,bar,baz],"foobarbaz",string).

test("simple concat, accept, #2") :-
   stringy_concat([foo,bar,baz],foobarbaz,atom).

test("simple concat, reject, #1",fail) :-
   stringy_concat([foo,bar,baz],"foob",string).

test("simple concat, reject, #2",fail) :-
   stringy_concat([foo,bar,baz],"foobarbaz2000",string).

test("simple concat to string") :-
   stringy_concat([foo,"bar",baz,"quux"],R,string),
   assertion(R == "foobarbazquux").

test("simple concat to either atom or string because type left unspecified") :-
   bagof([Type,R],stringy_concat([foo,"bar",baz,"quux"],R,Type),Bag),
   sort(Bag,Bag2),
   assertion(Bag2 == [[atom,'foobarbazquux'],[string,"foobarbazquux"]]).

test("simple concat to atom") :-
   stringy_concat([foo,"bar",baz,"quux"],R,atom),
   assertion(R == foobarbazquux).

test("simple concat to string, with empties") :-
   stringy_concat([foo,"bar",'',baz,"","quux"],R,string),
   assertion(R == "foobarbazquux").

test("simple concat to atom, with empties") :-
   stringy_concat([foo,"bar",'',baz,"","quux"],R,atom),
   assertion(R == foobarbazquux).

test("something other than a stringy in the list, default call",error(check(type,_,_,_))) :-
   stringy_concat([foo,1],_,string).

test("something other than a stringy in the list, soft call",error(check(type,_,_,_))) :-
   stringy_concat([foo,1],_,string).

test("something other than a stringy in the list, hard call",error(check(type,_,_,_))) :-
   stringy_concat([foo,1],_,string).

test("something other than a stringy as result, default call",fail) :-
   stringy_concat([foo,bar],1,string).

test("something other than a stringy as result, soft call",fail) :-
   stringy_concat([foo,bar],1,string,soft).

test("something other than a stringy as result, hard call",error(check(type,_,_,_))) :-
   stringy_concat([foo,bar],1,string,hard).

test("something other than a valid type as result, default call",fail) :-
   stringy_concat([foo,bar],foobar,1).

test("something other than a valid type as result, soft call",fail) :-
   stringy_concat([foo,bar],foobar,1,soft).

test("something other than a valid type as result, hard call",error(check(type,_,_,_))) :-
   stringy_concat([foo,bar],foobar,1,hard).

:- end_tests(stringy_concat).


