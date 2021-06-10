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

:- use_module(library('onepointfour_basics/stringy_morph.pl')).

:- begin_tests(stringy_morph).

test("Morph atom (arg 1) to atom or string") :-
   bagof([TypeA,StringyB:TypeB], stringy_morph(an_atom,StringyB,TypeA,TypeB), Bag),
   assertion(Bag = [[atom,an_atom:atom],[atom,"an_atom":string]]).

test("Morph string (arg 1) to atom or string") :-
   bagof([TypeA,StringyB:TypeB], stringy_morph("a_string",StringyB,TypeA,TypeB), Bag),
   assertion(Bag = [[string,"a_string":string],[string,a_string:atom]]).

test("Morph string (arg 2) to atom or string") :-
   bagof([StringyA:TypeA,TypeB], stringy_morph(StringyA,"a_string",TypeA,TypeB), Bag),
   assertion(Bag = [["a_string":string,string],[a_string:atom,string]]).

test("Morph atom (arg 2) to atom or string") :-
   bagof([StringyA:TypeA,TypeB], stringy_morph(StringyA,an_atom,TypeA,TypeB), Bag),
   assertion(Bag = [[an_atom:atom,atom],["an_atom":string,atom]]).

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

:- end_tests(stringy_morph).

% -----------------------------------------------------------------------------

:- begin_tests(stringy_charys_morph).

test("transform string to anything") :-
   bagof([StringyType,CharysType,Charys],
         stringy_charys_morph("hello",Charys,StringyType,CharysType,throw), 
         Bag),
   assertion( Bag == [ [string, char, [h,e,l,l,o]           ],
                       [string, code, [104,101,108,108,111] ] ] ).

test("transform atom to anything") :-
   bagof([StringyType,CharysType,Charys],
         stringy_charys_morph(hello,Charys,StringyType,CharysType,throw), Bag),
   assertion( Bag == [ [atom, char, [h,e,l,l,o]           ],
                       [atom, code, [104,101,108,108,111] ] ] ).

test("transform chars to anything") :-
   bagof([Stringy,StringyType,CharysType],
         stringy_charys_morph(Stringy,[h,e,l,l,o],StringyType,CharysType,throw), 
         Bag),
   assertion( Bag == [[hello,atom,char],["hello",string,char]] ).
                                              
test("transform codes to anything") :-
   bagof([Stringy,StringyType,CharysType],
         stringy_charys_morph(Stringy,[104,101,108,108,111],StringyType,CharysType,throw), 
         Bag),
   assertion( Bag == [[hello,atom,code],["hello",string,code]] ).

test("transform chars to atom") :-
   bagof(Stringy,stringy_charys_morph(Stringy,[h,e,l,l,o],atom,_,throw),Bag),
   assertion( Bag == [hello] ).
                                 
test("transform chars to string") :-
   bagof(Stringy,stringy_charys_morph(Stringy,[h,e,l,l,o],string,_,throw),Bag),
   assertion( Bag == ["hello"] ).

test("transform string to chars") :-
   bagof(Charys,stringy_charys_morph("hello",Charys,_,char,throw),Bag),
   assertion( Bag == [[h,e,l,l,o]] ).

test("transform string to codes") :-
   stringy_charys_morph("hello",Charys,_,code,throw),
   assertion( Charys == [104,101,108,108,111] ).
   
test("accept string/codes, types unspecified") :-
   stringy_charys_morph("hello",[104,101,108,108,111],T1,T2,throw),
   assertion( T1 == string ),
   assertion( T2 == code ).

test("accept string/codes, types specified") :-
   stringy_charys_morph("hello",[104,101,108,108,111],string,code,throw).
   
test("accept string/codes with codes semi-filled") :-
   stringy_charys_morph("hello",[104,X1,108,X2,111],T1,T2,throw),
   assertion( T1 == string ),
   assertion( T2 == code ),
   assertion( X1 == 101 ),
   assertion( X2 == 108 ).

test("accept string/chars with chars semi-filled") :-
   bagof([T1,T2,X1,X2],stringy_charys_morph("hello",[h,X1,l,X2,o],T1,T2,throw),Bag),
   assertion( Bag == [[string,char,e,l]] ).

test("transform no chars to empty atom") :-
   bagof([Stringy,CharysType],stringy_charys_morph(Stringy,[],atom,CharysType,throw),Bag),
   assertion( Bag == [['',char],['',code]] ).
                                 
test("transform no chars to empty string") :-
   bagof([Stringy,CharysType],stringy_charys_morph(Stringy,[],string,CharysType,throw),Bag),
   assertion( Bag == [["",char],["",code]] ).

test("transform no chars to empty stringys") :-
   bagof([Stringy,StringyType,CharysType],stringy_charys_morph(Stringy,[],StringyType,CharysType,throw),Bag),
   assertion( Bag == [['',atom,char],['',atom,code],["",string,char],["",string,code]] ).
 
:- end_tests(stringy_charys_morph).



