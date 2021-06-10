:- module(onepointfour_basics_stringy_morph,
          [
           stringy_morph/4         % stringy_morph(StringyA,StringyB,TypeA,TypeB)
          ,stringy_morph/5         % stringy_morph(StringyA,StringyB,TypeA,TypeB,Throw)
          ,stringy_charys_morph/4  % stringy_charys_morph(Stringy,Charys,StringyType,CharysType)
          ,stringy_charys_morph/5  % stringy_charys_morph(Stringy,Charys,StringyType,CharysType,Throw)
          ]).

:- use_module(library('onepointfour_basics/checks.pl')).
:- use_module(library('onepointfour_basics/stringy_and_charys_type.pl')).

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

/* <module> Simple predicates that try to make "manipulation of strings"
   and "manipulation of atoms" somewhat uniform.

## Tests

Running the tests: There should be a file =|stringy.plt|= nearby.

If you have loaded this module, a call to load_test_files/1 should run the tests:

```
?- use_module(library('onepointfour_basics/stringy.pl')).
?- load_test_files([]).
?- run_tests.
```

## History

- 2021-01-19: Reviewed
- 2021-02-16: Reviewed for packs, updated to pldoc.
- 2021-02-17: Trashed all the old code for testing "stringyness" and wrote new one based on the "texty type tree"

## About

   @license [MIT License](https://opensource.org/licenses/MIT)
   @author David Tonhofer (ronerycoder@gluino.name)

*/

%! stringy_morph(?StringyA,?StringyB,?TypeA,?TypeB)
%
% Behaves leniently, i.e. preferably fails instead of throwing if bad
% values are passed.

stringy_morph(StringyA,StringyB,TypeA,TypeB) :-
   stringy_morph(StringyA,StringyB,TypeA,TypeB,false).

%! stringy_morph(?StringyA,?StringyB,?TypeA,?TypeB,@Throw)
%
% Establish the "morph" relationship between StringyA (an atom or a string)
% and StringyB (an atom or a string) whereby StringyA and StringyB
% hold the same character sequence and the actual type of StringyA
% is given by TypeA, and the actual type of StringyB is given by
% TypeB (actual values of TypeA and TypeB are one of =|atom|= or
%  =|string|=). generate multiple solutions as possible.
%
% Throw is input only. If it is bound to =|throw|= or =|true|=, unwanted
% argument combinations lead to exception. If Throw has any other binding
% or is unbound, unwanted argument combinations lead to failure.

stringy_morph(StringyA,StringyB,TypeA,TypeB,Throw) :-
   check_that([StringyA,StringyB],[hard(passany(nonvar))]),
   check_that(StringyA,           [break(var),tuned(stringy)],Throw),
   check_that(StringyB,           [break(var),tuned(stringy)],Throw),
   check_that(TypeA,              [break(var),tuned(stringy_typeid)],Throw),
   check_that(TypeB,              [break(var),tuned(stringy_typeid)],Throw),
   stringy_type(StringyA,AsGivenTypeA),
   stringy_type(StringyB,AsGivenTypeB),
   stringy_morph_2(AsGivenTypeA,AsGivenTypeB,StringyA,StringyB,TypeA,TypeB).

% stringy_morph_2(AsGivenTypeA,AsGivenTypeB,StringyA,StringyB,TypeA,TypeB).
% The case AsGivenTypeA=var,AsGiventypeB=var is precluded by a check in stringy_morph/5

% We could put everything elegantly into a singly stringy_morph_2/6 table
% but to increase determinism we may call a secondary predicate.

% ------------  nonvar , nonvar          |- can be var on call -|
stringy_morph_2(string, string ,  _,  _,  string, string ).
stringy_morph_2(string, atom   ,  _,  _,  string, atom   ).
stringy_morph_2(atom,   string ,  _,  _,  atom  , string ).
stringy_morph_2(atom,   atom   ,  _,  _,  atom  , atom   ).
stringy_morph_2(atom,   var    ,  A,  B,  atom  , TypeB  ) :- stringy_morph_given_atom_as_A(TypeB,A,B).
stringy_morph_2(var,    atom   ,  A,  B,  TypeA , atom   ) :- stringy_morph_given_atom_as_A(TypeA,B,A).
stringy_morph_2(string, var    ,  A,  B,  string, TypeB  ) :- stringy_morph_given_string_as_A(TypeB,A,B).
stringy_morph_2(var,    string ,  A,  B,  TypeA , string ) :- stringy_morph_given_string_as_A(TypeA,B,A).

stringy_morph_given_atom_as_A(TypeB,A,B)  :-
   var(TypeB),
   !,
   ((TypeB=atom,A=B);(TypeB=string,atom_string(A,B))).
stringy_morph_given_atom_as_A(atom,A,A).
stringy_morph_given_atom_as_A(string,A,B) :-
   atom_string(A,B).

stringy_morph_given_string_as_A(TypeB,A,B)  :-
   var(TypeB),
   !,
   ((TypeB=string,A=B);(TypeB=atom,atom_string(B,A))).
stringy_morph_given_string_as_A(string,A,A).
stringy_morph_given_string_as_A(atom,A,B) :-
   atom_string(B,A).


%! stringy_charys_morph(Stringy,CharsOrCodes,WantStringy,WantCharsOrCodes)

stringy_charys_morph(Stringy,Charys,StringyType,CharysType) :-
   stringy_charys_morph(Stringy,Charys,StringyType,CharysType,false).

%! stringy_charys_morph(Stringy,CharsOrCodes,WantStringy,WantCharsOrCodes)
%
% Transform a stringy thing into a list of chars, or the reverse.
%
% This is not really needed as atom_length/2 and string_length/2 work for
% both strings and atoms, but it removes specificity of using atom_chars/2 
% or string_chars/2 from the program text.

stringy_charys_morph(Stringy,Charys,StringyType,CharysType,Throw) :-
   check_that(Stringy,     [break(var),tuned(stringy)],Throw),         % Stringy must be a var or an atom or a string
   check_that(StringyType, [break(var),tuned(stringy_typeid)],Throw),  % StringyType is a var or one of the atoms 'atom','string' 
   check_that(CharysType,  [break(var),tuned(chary_typeid)],Throw),    % CharysType is a var or one of the atoms 'char','code'
   (stringy_type_with_length(Stringy,ActualStringyType)                % won't fail because Stringy is well-typed
    ->
    true
    ;
    throw("Can't happen")),
   (charys_type(Charys,ActualCharysType)                               % may fail as Charys may be ill-typed TODO, catch that
    ->
    true
    ;    
    check_that(Charys,[fail("Charys has unrecognized type")],Throw)), 
   (\+underspecified(ActualStringyType,ActualCharysType) 
    ->
    true
    ;
    check_that([Stringy,Charys],[fail("Stringy-Charys combination underspecifies")],Throw)),
   (compatible_cross_combinations(ActualStringyType,ActualCharysType) 
    ->
    true
    ;
    check_that([ActualStringyType,ActualCharysType],[fail("Stringy-Charys type combination incompatible")],Throw)),
   enumerate_compatible_stringy_type(ActualStringyType,StringyType),  % fails on bad combination or may generate other solutions on redo
   enumerate_compatible_charys_type(ActualCharysType,CharysType),     % fails on bad combination or may generate other solutions on redo
   ((stringy_typeid\==var) 
     -> 
     morph_from_stringy(StringyType,CharysType,Stringy,Charys)     % morph nonvar "Stringy" to a charys according to "CharysType" and unify with "Charys", possibly yielding two solutions (codes,chars)
     ;
     morph_from_charys(CharysType,StringyType,Charys,Stringy)). % morph nonvar "Charys" to a stringy according to "StringyType" and unify with "Stringy", possibly yielding two solutions (atoms,strings)

% enumerate_compatible_stringy_type(ActualStringyType,StringyType)
% 
% This is an implementation of this table, where incompatible
% combinations have been left out and thus fail
%
% ActualStringyType  StringyType
% ------------------------------
% 'var'              Var,'atom','string'   StringyType can be bound to 'atom' or 'string'
% 'atom(L)'          Var,'atom'            StringyType must be bound to 'atom'
% 'string(L)'        Var,'string'          StringyType must be bound to 'string'

enumerate_compatible_stringy_type(string(_),string).
enumerate_compatible_stringy_type(atom(_),atom).
enumerate_compatible_stringy_type(var,atom).
enumerate_compatible_stringy_type(var,string).

% enumerate_compatible_charys_type(ActualyCharysType,CharysType)

enumerate_compatible_charys_type(chars(_),char).
enumerate_compatible_charys_type(codes(_),code).
enumerate_compatible_charys_type(chars_vars(_,_),char).
enumerate_compatible_charys_type(codes_vars(_,_),code).
enumerate_compatible_charys_type(empty,char).
enumerate_compatible_charys_type(empty,code).
enumerate_compatible_charys_type(vars(_),char).
enumerate_compatible_charys_type(vars(_),code).
enumerate_compatible_charys_type(var,char).
enumerate_compatible_charys_type(var,code).

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

% morph_from_stringy(StringyType,CharysType,StringyIn,CharysOut)
 
morph_from_stringy(string , char , StringyIn , CharysOut) :- string_chars(StringyIn,CharysOut).
morph_from_stringy(string , code , StringyIn , CharysOut) :- string_codes(StringyIn,CharysOut).
morph_from_stringy(atom   , char , StringyIn , CharysOut) :- atom_chars(StringyIn,CharysOut).
morph_from_stringy(atom   , code , StringyIn , CharysOut) :- atom_codes(StringyIn,CharysOut).

% morph_from_charys(CharysType,StringyType,CharysIn,StringyOut)
 
morph_from_charys(chars(_) , string , CharysIn, StringyOut) :- string_chars(StringyOut,CharysIn).
morph_from_charys(chars(_) , atom   , CharysIn, StringyOut) :- atom_chars(StringyOut,CharysIn).
morph_from_charys(codes(_) , string , CharysIn, StringyOut) :- string_codes(StringyOut,CharysIn).
morph_from_charys(codes(_) , atom   , CharysIn, StringyOut) :- atom_codes(StringyOut,CharysIn).
morph_from_charys(empty    , string , [], "").
morph_from_charys(empty    , atom   , [], '').

