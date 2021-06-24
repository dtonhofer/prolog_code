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

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_space_stringy.md
*/

:- use_module(library('onepointfour_basics/space_stringy.pl')).

:- begin_tests(space_stringy).

% --- generate normally

test("generate a short string") :-
   space_stringy(5,Str,string),
   assertion(Str == "     ").

test("generate a short atom") :-
   space_stringy(5,Str,atom),
   assertion(Str == '     ').

test("generate a zero-length string") :-
   space_stringy(0,Str,string),
   assertion(Str == "").

test("generate a zero-length atom") :-
   space_stringy(0,Str,atom),
   assertion(Str == '').

% --- accept normally

test("verify that a stringy has a certain length and type #1") :-
   space_stringy(5,"     ",string).

test("verify that a stringy has a certain length and type #2") :-
   space_stringy(5,'     ',atom).

% --- analyze normally

test("obtain a stringy's length and type #1") :-
   space_stringy(N,"     ",T),
   assertion(N==5),
   assertion(T==string).

test("obtain a stringy's length and type #1") :-
   space_stringy(N,'     ',T),
   assertion(N==5),
   assertion(T==atom).

% --- do not accept non-space strings

test("fail for a non-space string",fail) :-
   space_stringy(_,"Hello World",_).

test("fail for a non-space atom",fail) :-
   space_stringy(_,'Hello World',_).

% --- do not accept strings if the type is wrong

test("fail for a string when the type is given as 'atom'",fail) :-
   space_stringy(_,"         ",atom).

test("fail for an atom when the type is given as 'string'",fail) :-
   space_stringy(_,'         ',string).

% --- do not accept on bad length

test("fail at bad length of string",fail) :-
   space_stringy(50,"     ",_).

% --- negative length

test("fail on negative length if smooth",fail) :-
   space_stringy_smooth(-1,_Str,string).

test("fail on negative length if tuned to soft",fail) :-
   space_stringy(-1,_Str,string,soft).

test("throw on negative length if tuned to throw",error(check(domain,_,_,_))) :-
   space_stringy(-1,_Str,string,hard).

test("generate empty string on negative length if lax") :-
   space_stringy_lax(-1,Str,string),
   assertion(Str == "").

% --- stringy is out of type

test("fail on non-string if smooth",fail) :-
   space_stringy_smooth(_,[],_).

test("throw on non-string if standard",error(check(type,_,_,_))) :-
   space_stringy(_,[],_).

test("throw on non-string if tuned to throw",error(_,_)) :-
   space_stringy(_,[],_,hard).

% --- not passing an inetger

test("throw on non-integer",error(check(type,_,_,_))) :-
   space_stringy(foo,"   ",string).

test("fail on non-integer if smooth",fail) :-
   space_stringy_smooth(foo,"   ",string).

% --- both string and type not given (but length given)

test("only length") :-
   bagof([Str,Type],space_stringy(2,Str,Type),Bag),
   sort(Bag,Bag2),
   assertion(Bag2 == [["  ",string],
                      ['  ',atom]]).

% --- generate strings or atoms

test("only type 'strings' given: generate increasingly long strings") :-
   bagof(Len-Str,limit(5,space_stringy(Len,Str,string)),Bag),
   assertion(Bag == [0-"",1-" ",2-"  ",3-"   ",4-"    "]).

test("only type 'atoms' given: generate increasingly long atoms") :-
   bagof(Len-Str,limit(5,space_stringy(Len,Str,atom)),Bag),
   assertion(Bag == [0-'',1-' ',2-'  ',3-'   ',4-'    ']).

test("nothing given: generate increasingly long atoms and strings") :-
   bagof([Len,Str,Type],limit(10,space_stringy(Len,Str,Type)),Bag),
   sort(Bag,Bag2),
   assertion(Bag2 == [[0,"",string],    [0,'',atom],
                      [1," ",string],   [1,' ',atom],
                      [2,"  ",string],  [2,'  ',atom],
                      [3,"   ",string], [3,'   ',atom],
                      [4,"    ",string],[4,'    ',atom]]).

% --- generate lots

test("generate multiple strings") :-
   forall(
      between(0,2000,Len),
      (
         space_stringy(Len,Spaces,string),    % generation
         string_length(Spaces,Len)           % verification
      )
   ).

:- end_tests(space_stringy).

