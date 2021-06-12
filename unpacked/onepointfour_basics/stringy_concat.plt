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

test("empty list 1") :-
   stringy_concat([],S,atom),
   assertion(S == '').

test("empty list 2") :-
   stringy_concat([],S,string),
   assertion(S == "").

test("empty list 3") :-
   stringy_concat([],"",Type),
   assertion(Type == string).

test("empty list 4") :-
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

test("simple concat to atom") :-
   stringy_concat([foo,"bar",baz,"quux"],R,atom),
   assertion(R == foobarbazquux).

test("simple concat to string, with empties") :-
   stringy_concat([foo,"bar",'',baz,"","quux"],R,string),
   assertion(R == "foobarbazquux").

test("simple concat to atom, with empties") :-
   stringy_concat([foo,"bar",'',baz,"","quux"],R,atom),
   assertion(R == foobarbazquux).

:- end_tests(stringy_concat).


