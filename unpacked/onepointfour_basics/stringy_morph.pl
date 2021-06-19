:- module(onepointfour_basics_stringy_morph,
          [
           stringy_morph/4            % stringy_morph(StringyA,StringyB,TypeA,TypeB)
          ,stringy_morph/5            % stringy_morph(StringyA,StringyB,TypeA,TypeB,Tuned)
          ,stringy_charylist_morph/4  % stringy_charylist_morph(Stringy,Charylist,StringyType,CharylistType)
          ,stringy_charylist_morph/5  % stringy_charylist_morph(Stringy,Charylist,StringyType,CharylistType,Tuned)
          ]).

:- use_module(library('onepointfour_basics/checks.pl')).
:- use_module(library('onepointfour_basics/stringy_and_charylist_type.pl')).

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

/* pldoc ==================================================================== */

/** <module> A replacement for atom_string/2

This code is specific to SWI-Prolog, as that Prolog provides the traditional
"atom" and the non-traditional "string" as two distinct representations of
"sequences of characters".

We introduce the following additional vocabulary:

- A stringy term is a term that is either an atom or a string.
  In SWI-Prolog, the string is a distinct representation of a sequence
  of characters, distinct from the atom and mean to be used in text
  processing rather than as basis for identifiers.
- A chary term is a term that is either a char (an atom of length 1) or a
  code (an integer and, more precisely in SWI-Prolog, a Unicode code point).
- A charylist is less precise: it is a proper list of either codes or chars.
  It may or may not contain uninstantiated elements. An empty list is a
  charylist but we cannot know whether it is supposed to be composed of
  codes or chars. A list containing only uninstantiated variables is also
  a charylist and again we don't know what it is supposed to contain, at
  least not yet.

## Homepage for this code

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_morph.md

## History

- 2021-01-19: Reviewed
- 2021-02-16: Reviewed for packs, updated to pldoc.
- 2021-02-17: Trashed all the old code for testing "stringyness" and wrote new one based on the "texty type tree"
- 2021-06-11: Back up on github

*/

%! stringy_morph(?StringyA,?StringyB,?TypeA,?TypeB)
%
% Behaves leniently, i.e. preferably fails instead of throwing if bad
% values are passed.

stringy_morph(StringyA,StringyB,TypeA,TypeB) :-
   stringy_morph(StringyA,StringyB,TypeA,TypeB,soft).

%! stringy_morph(?StringyA,?StringyB,?TypeA,?TypeB,@Tuned)
%
% Establish the "morph" relationship between StringyA (an atom or a string)
% and StringyB (an atom or a string) whereby
%
% - StringyA and StringyB hold the same character sequence, and
% - the actual type of StringyA is given by TypeA, and
% - the actual type of StringyB is given by TypeB
%
% Actual values of TypeA and TypeB are one of =|atom|= or =|string|=.
%
% Generate multiple solutions as possible.
%
% Tuned is input only. If it is bound to =|hard|=, unwanted
% argument combinations lead to exception. If is bound to =|soft|=
% (actually, anything other than =|hard|=.
% unwanted argument combinations lead to failure.

stringy_morph(StringyA,StringyB,TypeA,TypeB,Tuned) :-
   check_that([StringyA,StringyB],[hard(passany(nonvar))]),
   check_that(StringyA,           [break(var),tuned(stringy)],Tuned),
   check_that(StringyB,           [break(var),tuned(stringy)],Tuned),
   check_that(TypeA,              [break(var),tuned(stringy_typeid)],Tuned),
   check_that(TypeB,              [break(var),tuned(stringy_typeid)],Tuned),
   stringy_type(StringyA,AsGivenTypeA),
   stringy_type(StringyB,AsGivenTypeB),
   stringy_morph_2_with_increased_determinism(AsGivenTypeA,AsGivenTypeB,StringyA,StringyB,TypeA,TypeB).

% stringy_morph_2(AsGivenTypeA,AsGivenTypeB,StringyA,StringyB,TypeA,TypeB).
%
% The case AsGivenTypeA=var,AsGiventypeB=var is precluded by a check in
% stringy_morph/5
%
% Note that even if the types correspond, we must still check whether
% the text representations correspond.

stringy_morph_2_with_increased_determinism(AsGivenTypeA,AsGivenTypeB,StringyA,StringyB,TypeA,TypeB) :-
   (
      (AsGivenTypeA==var,nonvar(TypeA));
      (AsGivenTypeB==var,nonvar(TypeB))
   ),
   !,
   stringy_morph_2(AsGivenTypeA,AsGivenTypeB,StringyA,StringyB,TypeA,TypeB), % we know there is only 1 solution
   !.
stringy_morph_2_with_increased_determinism(AsGivenTypeA,AsGivenTypeB,StringyA,StringyB,TypeA,TypeB) :-
   stringy_morph_2(AsGivenTypeA,AsGivenTypeB,StringyA,StringyB,TypeA,TypeB).

% stringy_morph_2(AsGivenTypeA,AsGivenTypeB,StringyA,StringyB,TypeA,TypeB).
% ------------  | nonvar on call |           | can be var on call |

stringy_morph_2( string, string   ,  A , B ,    string, string ) :- A=B.
stringy_morph_2( string, atom     ,  A , B ,    string, atom   ) :- atom_string(B,A).
stringy_morph_2( string, var      ,  A , B ,    string, string ) :- A=B.
stringy_morph_2( string, var      ,  A , B ,    string, atom   ) :- atom_string(B,A).

stringy_morph_2( atom,   string   ,  A , B ,    atom  , string ) :- atom_string(A,B).
stringy_morph_2( atom,   atom     ,  A , B ,    atom  , atom   ) :- A=B.
stringy_morph_2( atom,   var      ,  A , B ,    atom  , string ) :- atom_string(A,B).
stringy_morph_2( atom,   var      ,  A , B ,    atom  , atom   ) :- A=B.

stringy_morph_2( var,    atom     ,  A , B ,    string, atom   ) :- atom_string(B,A).
stringy_morph_2( var,    atom     ,  A , B ,    atom  , atom   ) :- A=B.
stringy_morph_2( var,    string   ,  A , B ,    string, string ) :- A=B.
stringy_morph_2( var,    string   ,  A , B ,    atom  , string ) :- atom_string(A,B).

%! stringy_charylist_morph(Stringy,Charylist,StatedStringyType,StatedCharylistType)

stringy_charylist_morph(Stringy,Charylist,StatedStringyType,StatedCharylistType) :-
   stringy_charylist_morph(Stringy,Charylist,StatedStringyType,StatedCharylistType,soft).

%! stringy_charylist_morph(Stringy,Charylist,WantStringy,WantCharylist)
%
% Map a stringy to a charylist.

stringy_charylist_morph(Stringy,Charylist,StatedStringyType,StatedCharylistType,Tuned) :-
   check_that(Stringy,             [break(var),tuned(stringy)],Tuned),
   check_that(StatedStringyType,   [break(var),tuned(stringy_typeid)],Tuned),
   check_that(StatedCharylistType, [break(var),tuned(member([char,code,chars,codes]))],Tuned),
   fix(StatedCharylistType,StatedCharylistType2),
   % Won't fail because Stringy is well-typed after check_that/2
   stringy_type_with_length(Stringy,ActualStringyType),
   % May fail as Charylist may be ill-typed
   (charylist_type(Charylist,ActualCharylistType)
    ->
    true
    ;
    check_that(Charylist,[fail("Charylist has unrecognized type")],Tuned)),
   (\+underspecified(ActualStringyType,ActualCharylistType)
    ->
    true
    ;
    check_that([Stringy,Charylist],[fail("Stringy-Charylist combination underspecifies")],Tuned)),
   (compatible_cross_combinations(ActualStringyType,ActualCharylistType)
    ->
    true
    ;
    check_that([ActualStringyType,ActualCharylistType],[fail("Stringy-Charylist type combination incompatible")],Tuned)),
   % below here it's gettign interesting; the cur is really not needed
   % fails on bad combination or may generate other solutions on redo
   enumerate_compatible_stringy_type_with_increased_determinism(ActualStringyType,StatedStringyType),
   % fails on bad combination or may generate other solutions on redo
   enumerate_compatible_charylist_type_with_increased_determinism(ActualCharylistType,StatedCharylistType2),
   % we make the exercise of explicitly calling the correct builtin depending on case
   ((ActualStringyType==var)
      ->
      % morph nonvar "Charylist" to a stringy according to "StringyType" and unify
      % with "Stringy", possibly yielding two solutions (atoms,strings)
      morph_from_charylist_with_increased_determinism(StatedCharylistType2,StatedStringyType,Charylist,Stringy)
      ;
      % morph nonvar "Stringy" to a charylist according to "StatedCharylistType2" and
      % unify with "Charylist", possibly yielding two solutions (codes,chars)
      morph_from_stringy_with_increased_determinism(StatedStringyType,StatedCharylistType2,Stringy,Charylist)).


% fix(StatedCharylistType,Fixed)
% The caller may have passed any of char,chars,code,codes; unify!

fix(X,X)          :- var(X),!.
fix(chars,chars)  :- !.
fix(char,chars)   :- !.
fix(codes,codes)  :- !.
fix(code,codes).

% enumerate_compatible_stringy_type(ActualStringyType,StatedStringyType)
%
% This is an implementation of this table, where incompatible
% combinations have been left out and thus fail
%
% ActualStringyType  StatedStringyType (may be var)
% ------------------------------
% 'var'              Var,'atom','string'   StringyType can be bound to 'atom' or 'string'
% 'atom(L)'          Var,'atom'            StringyType must be bound to 'atom'
% 'string(L)'        Var,'string'          StringyType must be bound to 'string'
%

enumerate_compatible_stringy_type_with_increased_determinism(A,B) :-
   assertion(nonvar(A)),
   nonvar(B),
   !,
   enumerate_compatible_stringy_type(A,B),
   !.
enumerate_compatible_stringy_type_with_increased_determinism(A,B) :-
   enumerate_compatible_stringy_type(A,B).

enumerate_compatible_stringy_type(string(_),string).
enumerate_compatible_stringy_type(atom(_),atom).
enumerate_compatible_stringy_type(var,atom).
enumerate_compatible_stringy_type(var,string).

% enumerate_compatible_charylist_type(ActualCharylistType,StatedCharylistType)

enumerate_compatible_charylist_type_with_increased_determinism(A,B) :-
   assertion(nonvar(A)),
   nonvar(B),
   !,
   enumerate_compatible_charylist_type(A,B),
   !.
enumerate_compatible_charylist_type_with_increased_determinism(A,B) :-
   enumerate_compatible_charylist_type(A,B).

enumerate_compatible_charylist_type(chars(_),chars).
enumerate_compatible_charylist_type(codes(_),codes).
enumerate_compatible_charylist_type(chars_vars(_,_),chars).
enumerate_compatible_charylist_type(codes_vars(_,_),codes).
enumerate_compatible_charylist_type(empty,chars).
enumerate_compatible_charylist_type(empty,codes).
enumerate_compatible_charylist_type(vars(_),chars).
enumerate_compatible_charylist_type(vars(_),codes).
enumerate_compatible_charylist_type(var,chars).
enumerate_compatible_charylist_type(var,codes).

% underspecified(ActualStringyType,ActualCharysType)
%
% List the type combinations which leave too much leeway for a meaningful answer.

underspecified(var,var).
underspecified(var,vars(_)).
underspecified(var,chars_vars(_,_)).
underspecified(var,codes_vars(_,_)).

% compatible_cross_combinations(ActualStringyType,ActualCharysType)
%
% List the type combinations for which Stringy<->Charys morph makes sense.

compatible_cross_combinations(string(L),chars(L)).
compatible_cross_combinations(string(L),codes(L)).
compatible_cross_combinations(atom(L),chars(L)).
compatible_cross_combinations(atom(L),codes(L)).
compatible_cross_combinations(string(L),chars_vars(C,V)) :- L =:= C+V.
compatible_cross_combinations(string(L),codes_vars(C,V)) :- L =:= C+V.
compatible_cross_combinations(atom(L),chars_vars(C,V)) :- L =:= C+V.
compatible_cross_combinations(atom(L),codes_vars(C,V)) :- L =:= C+V.
compatible_cross_combinations(string(0),empty).
compatible_cross_combinations(atom(0),empty).
compatible_cross_combinations(string(L),vars(L)).
compatible_cross_combinations(atom(L),vars(L)).
compatible_cross_combinations(string(_),var).
compatible_cross_combinations(atom(_),var).

compatible_cross_combinations(var,chars(_)).
compatible_cross_combinations(var,codes(_)).
compatible_cross_combinations(var,empty).

% morph_from_stringy(StringyType,CharylistType,StringyIn,CharylistOut)
% More complex than need to get determinism on the first two arguments

morph_from_stringy_with_increased_determinism(A,B,C,D) :-
   assertion(nonvar(A)),
   nonvar(B),
   !,
   morph_from_stringy(A,B,C,D),
   !.
morph_from_stringy_with_increased_determinism(A,B,C,D) :-
   morph_from_stringy(A,B,C,D).

morph_from_stringy(string , chars , StringyIn , CharylistOut) :- string_chars(StringyIn,CharylistOut).
morph_from_stringy(string , codes , StringyIn , CharylistOut) :- string_codes(StringyIn,CharylistOut).
morph_from_stringy(atom   , chars , StringyIn , CharylistOut) :- atom_chars(StringyIn,CharylistOut).
morph_from_stringy(atom   , codes , StringyIn , CharylistOut) :- atom_codes(StringyIn,CharylistOut).

% morph_from_charylist(CharylistType,StringyType,CharylistIn,StringyOut)

morph_from_charylist_with_increased_determinism(A,B,C,D) :-
   assertion(nonvar(A)),
   nonvar(B),
   !,
   morph_from_charylist(A,B,C,D),
   !.
morph_from_charylist_with_increased_determinism(A,B,C,D) :-
   morph_from_charylist(A,B,C,D).

morph_from_charylist(chars , string , CharylistIn, StringyOut) :- string_chars(StringyOut,CharylistIn).
morph_from_charylist(chars , atom   , CharylistIn, StringyOut) :- atom_chars(StringyOut,CharylistIn).
morph_from_charylist(codes , string , CharylistIn, StringyOut) :- string_codes(StringyOut,CharylistIn).
morph_from_charylist(codes , atom   , CharylistIn, StringyOut) :- atom_codes(StringyOut,CharylistIn).
morph_from_charylist(empty , string , [], "").
morph_from_charylist(empty , atom   , [], '').


