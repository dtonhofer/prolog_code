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

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_morph.md
*/

:- use_module(library('onepointfour_basics/stringy_morph.pl')).

:- begin_tests(stringy_morph).

test("Morph atom (arg 1) to atom or string") :-
   bagof([TypeA,StringyB:TypeB], stringy_morph(an_atom,StringyB,TypeA,TypeB), Bag),
   assertion(Bag = [
      [atom,"an_atom":string],
      [atom,an_atom:atom]
   ]).

test("Morph string (arg 1) to atom or string") :-
   bagof([TypeA,StringyB:TypeB], stringy_morph("a_string",StringyB,TypeA,TypeB), Bag),
   assertion(Bag = [[string,"a_string":string],[string,a_string:atom]]).

test("Morph string (arg 2) to atom or string") :-
   bagof([StringyA:TypeA,TypeB], stringy_morph(StringyA,"a_string",TypeA,TypeB), Bag),
   assertion(Bag = [["a_string":string,string],[a_string:atom,string]]).

test("Morph atom (arg 2) to atom or string") :-
   bagof([StringyA:TypeA,TypeB], stringy_morph(StringyA,an_atom,TypeA,TypeB), Bag),
   assertion(Bag = [
      ["an_atom":string,atom],
      [an_atom:atom,atom]
   ]).

test("atom (arg 2) to string (arg 1)") :-
   stringy_morph(StringyA,an_atom,string,_),
   assertion(StringyA == "an_atom").

test("atom (arg 1) to string (arg 2)") :-
   stringy_morph(an_atom,StringyB,_,string),
   assertion(StringyB == "an_atom").

test("string (arg 2) to string (arg 1)") :-
   stringy_morph(StringyA,"a string",string,_),
   assertion(StringyA == "a string").

test("string (arg 1) to string (arg 2)") :-
   stringy_morph("a string",StringyB,_,string),
   assertion(StringyB == "a string").

test("string (arg 2) to atom (arg 1)") :-
   stringy_morph(StringyA,"a string",atom,_),
   assertion(StringyA == 'a string').

test("string (arg 1) to atom (arg 2)") :-
   stringy_morph("a string",StringyB,_,atom),
   assertion(StringyB == 'a string').

test("atom (arg 2) to atom (arg 1)") :-
   stringy_morph(StringyA,an_atom,atom,_),
   assertion(StringyA == an_atom).

test("atom (arg 1) to atom (arg 2)") :-
   stringy_morph(an_atom,StringyB,_,atom),
   assertion(StringyB == an_atom).

test("nonmatching types #1",fail) :-
   stringy_morph(an_atom,an_atom,string,atom).

test("nonmatching types #2",fail) :-
   stringy_morph(an_atom,an_atom,atom,string).

test("nonmatching types #3",fail) :-
   stringy_morph(an_atom,an_atom,string,string).

test("bad type key #1, soft mode fails",fail) :-
   stringy_morph(an_atom,an_atom,foo,foo).

test("bad type key #1, hard mode throws",error(check(domain,_,_,_))) :-
   stringy_morph(an_atom,an_atom,foo,foo,throw).

test("bad type key #2, soft mode fails",fail) :-
   stringy_morph(an_atom,an_atom,777,foo).

test("bad type key #2, hard mode throws",error(check(type,_,_,_))) :-
   stringy_morph(an_atom,an_atom,777,foo,throw).

test("non-stringy value, soft mode fails",fail) :-
   stringy_morph(777,an_atom,_,_).

test("non-stringy value, hard mode throws",error(check(type,_,_,_))) :-
   stringy_morph(777,an_atom,_,_,throw).

test("two vars instead of at least one stringy, soft mode throws",error(check(passany,_,_,_))) :-
   stringy_morph(_,_,string,string).

test("two vars instead of at least one string, hard mode throws",error(check(passany,_,_,_))) :-
   stringy_morph(_,_,string,string,true).

test("types correctly fully specified, stringys don't match #1", fail) :-
   stringy_morph(hello,"world",atom,string).

test("types correctly fully specified, stringys don't match #2", fail) :-
   stringy_morph(hello,world,atom,atom).

test("types correctly fully specified, stringys do match #1") :-
   stringy_morph(hello,"hello",atom,string).

test("types correctly fully specified, stringys do match #2") :-
   stringy_morph(hello,hello,atom,atom).

:- end_tests(stringy_morph).

% -----------------------------------------------------------------------------

:- begin_tests(stringy_charylist_morph).

test("transform string to anything") :-
   bagof([StringyType,CharylistType,Charylist],
         stringy_charylist_morph("hello",Charylist,StringyType,CharylistType,throw),
         Bag),
   assertion( Bag == [ [string, chars, [h,e,l,l,o]           ],
                       [string, codes, [104,101,108,108,111] ] ] ).

test("transform atom to anything") :-
   bagof([StringyType,CharylistType,Charylist],
         stringy_charylist_morph(hello,Charylist,StringyType,CharylistType,throw), Bag),
   assertion( Bag == [ [atom, chars, [h,e,l,l,o]           ],
                       [atom, codes, [104,101,108,108,111] ] ] ).

test("transform chars to anything") :-
   bagof([Stringy,StringyType,CharylistType],
         stringy_charylist_morph(Stringy,[h,e,l,l,o],StringyType,CharylistType,throw),
         Bag),
   assertion( Bag == [ [hello,atom,chars],
                       ["hello",string,chars] ] ).

test("transform codes to anything") :-
   bagof([Stringy,StringyType,CharylistType],
         stringy_charylist_morph(Stringy,[104,101,108,108,111],StringyType,CharylistType,throw),
         Bag),
   assertion( Bag == [ [hello,atom,codes],
                       ["hello",string,codes] ] ).

test("transform chars to atom") :-
   stringy_charylist_morph(Stringy,[h,e,l,l,o],atom,_,throw),
   assertion( Stringy == hello ).

test("transform chars to string") :-
   stringy_charylist_morph(Stringy,[h,e,l,l,o],string,_,throw),
   assertion( Stringy == "hello" ).

test("transform string to chars") :-
   stringy_charylist_morph("hello",Charylist,_,chars,throw),
   assertion( Charylist == [h,e,l,l,o] ).

test("transform string to codes") :-
   stringy_charylist_morph("hello",Charylist,_,codes,throw),
   assertion( Charylist == [104,101,108,108,111] ).

test("accept string/codes, types unspecified") :-
   stringy_charylist_morph("hello",[104,101,108,108,111],T1,T2,throw),
   assertion( T1 == string ),
   assertion( T2 == codes ).

test("accept string/codes, types specified") :-
   stringy_charylist_morph("hello",[104,101,108,108,111],string,codes,throw).

test("accept string/codes with codes semi-filled") :-
   stringy_charylist_morph("hello",[104,X1,108,X2,111],T1,T2,throw),
   assertion( [T1,T2,X1,X2] == [string,codes,101,108] ).

test("accept string/chars with chars semi-filled") :-
   stringy_charylist_morph("hello",[h,X1,l,X2,o],T1,T2,throw),
   [T1,T2,X1,X2] == [string,chars,e,l].

test("transform no chars to empty atom") :-
   bagof([Stringy,CharylistType],stringy_charylist_morph(Stringy,[],atom,CharylistType,throw),Bag),
   assertion( Bag == [['',chars],['',codes]] ).

test("transform no chars to empty string") :-
   bagof([Stringy,CharylistType],stringy_charylist_morph(Stringy,[],string,CharylistType,throw),Bag),
   assertion( Bag == [["",chars],["",codes]] ).

test("transform no chars to empty stringys") :-
   bagof([Stringy,StringyType,CharylistType],stringy_charylist_morph(Stringy,[],StringyType,CharylistType,throw),Bag),
   assertion( Bag == [['',atom,chars],['',atom,codes],["",string,chars],["",string,codes]] ).

:- end_tests(stringy_charylist_morph).



