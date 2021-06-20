:- module(onepointfour_basics_stringy_and_charylist_type,
          [
           charylist_type/2
          ,stringy_type/2
          ,stringy_type/3
          ,stringy_type_with_length/2
          ,stringy_type_with_length/3
          ]).

:- use_module(library('onepointfour_basics/checks.pl')).

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

/** <module> Analyze "chary" or "stringy" terms

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

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_and_charylist_type.md

## History

   1. 2020-07-XX: First code elements created.
   1. 2021-06-08: Re-created from existing code lying around.
   1. 2021-06-11: Back up on github.

*/

%! charylist_type(@CharyList,?Type)
%
% Determine the type of a charylist. It will be one of the following atoms
% or compound terms **or else the predicate will fail** (it doesn't throw,
% i.e. behaves "smoothly"). Note that CharyList must be proper list, partial
% lists are rejected.
%
% - =|var|=              : charylist is uninstantiated
% - =|empty|=            : charylist is the empty list (no info whether chars or codes)
% - =|chars(N)|=         : charylist is nonempty and consists of N chars
% - =|codes(N)|=         : charylist is nonempty and consists of N codes
% - =|chars_vars(N,V)|=  : charylist is nonempty and consists of N chars and V vars
% - =|codes_vars(N,V)|=  : charylist is nonempty and consists of N codes and V vars
% - =|vars(V)|=          : charylist is nonempty and consists of V vars

charylist_type(CharyList,var) :-
   var(CharyList),
   !.

charylist_type(CharyList,Type) :-
   list_traversal(CharyList,VarCount,CodeCount,CharCount,TotalCount), % fails on malformed/non-list
   assertion(VarCount + CodeCount + CharCount =:= TotalCount),
   type_decide(VarCount,CodeCount,CharCount,Type).

type_decide(VarCount,CodeCount,CharCount,chars_vars(CharCount,VarCount)) :-
   VarCount > 0,
   CharCount > 0,
   assertion(CodeCount == 0),
   !.

type_decide(VarCount,CodeCount,CharCount,codes_vars(CodeCount,VarCount)) :-
   VarCount > 0,
   CodeCount > 0,
   assertion(CharCount == 0),
   !.

type_decide(VarCount,CodeCount,CharCount,vars(VarCount)) :-
   VarCount > 0,
   CodeCount == 0,
   CharCount == 0,
   !.

type_decide(VarCount,CodeCount,CharCount,vars(VarCount)) :-
   VarCount > 0,
   CodeCount == 0,
   CharCount == 0,
   !.

type_decide(VarCount,CodeCount,CharCount,chars(CharCount)) :-
   VarCount == 0,
   CharCount > 0,
   assertion(CodeCount == 0),
   !.

type_decide(VarCount,CodeCount,CharCount,codes(CodeCount)) :-
   VarCount == 0,
   CodeCount > 0,
   assertion(CharCount == 0),
   !.

type_decide(VarCount,CodeCount,CharCount,empty) :-
   assertion(VarCount + CodeCount + CharCount =:= 0).

list_traversal(List,VarCount,CodeCount,CharCount,TotalCount) :-
   list_traversal_2(List,0,0,0,0,VarCount,CodeCount,CharCount,TotalCount).

% we need to beware of being handed an open list, so we
% can't unify solely in the head

list_traversal_2(List,VarCount,CodeCount,CharCount,TotalCount,VarCountOut,CodeCountOut,CharCountOut,TotalCountOut) :-
   nonvar(List),
   List=[X|More],
   var(X),
   !,
   VarCountNext is VarCount+1,
   TotalCountNext is TotalCount+1,
   list_traversal_2(More,VarCountNext,CodeCount,CharCount,TotalCountNext,
                         VarCountOut,CodeCountOut,CharCountOut,TotalCountOut).

list_traversal_2(List,VarCount,CodeCount,CharCount,TotalCount,VarCountOut,CodeCountOut,CharCountOut,TotalCountOut) :-
   nonvar(List),
   List=[X|More],
   check_that(X,[smooth(char)]), % is a char, fail if not
   !,
   CodeCount=:=0,
   CharCountNext is CharCount+1,
   TotalCountNext is TotalCount+1,
   list_traversal_2(More,VarCount,CodeCount,CharCountNext,TotalCountNext,
                         VarCountOut,CodeCountOut,CharCountOut,TotalCountOut).

list_traversal_2(List,VarCount,CodeCount,CharCount,TotalCount,VarCountOut,CodeCountOut,CharCountOut,TotalCountOut) :-
   nonvar(List),
   List=[X|More],
   check_that(X,[smooth(code)]), % is a code, fail if not
   !,
   CharCount=:=0,
   CodeCountNext is CodeCount+1,
   TotalCountNext is TotalCount+1,
   list_traversal_2(More,VarCount,CodeCountNext,CharCount,TotalCountNext,
                         VarCountOut,CodeCountOut,CharCountOut,TotalCountOut).

list_traversal_2(List,VarCount,CodeCount,CharCount,TotalCount,VarCount,CodeCount,CharCount,TotalCount) :-
   nonvar(List),
   List=[].

%! stringy_type(@Stringy,?Type)
%
% Determine the type of Stringy. Type can be =|string|= or =|atom|= or =|var|=, the
% latter indicating that Stringy is uninstantiated. This predicates behaves _softly_,
% i.e. preferentially fails on bad input.

stringy_type(Stringy,Type) :-
   stringy_type(Stringy,Type,soft).

% !stringy_type(@Stringy,?Type,@Tuned)
%
% As stringy_type/2, but setting Tuned to =|hard|= will make the
% predicate throw on bad input (contrary is =|soft|=)

stringy_type(Stringy,Type,Tuned) :-
   check_that(Stringy,[break(var),tuned(stringy)],Tuned),
   check_that(Type,[break(var),tuned(member([var,atom,string]))],Tuned),
   stringy_type_2(Stringy,Type).

stringy_type_2(Stringy,var)    :- var(Stringy),!.
stringy_type_2(Stringy,atom)   :- atom(Stringy),!.
stringy_type_2(Stringy,string) :- string(Stringy),!.

%! stringy_type_with_length(@Stringy,Type)
%
% Determine an atom or compound-term representation for the actual type of Stringy.
% It will be one of the atom =|var|= or one of the compound terms =|atom(L)|= or
% =|string(L)|=, where L is the length of Stringy. This predicates behaves _softly_,
% i.e. preferentially fails on bad input.

stringy_type_with_length(Stringy,Type) :-
   stringy_type_with_length(Stringy,Type,soft).

%! stringy_type_with_length(@Stringy,Type,Tuned)
%
% As stringy_type_with_length/2, but setting Tuned to either =|true|= or =|throw|=
% will make the predicate throw on bad input.

stringy_type_with_length(Stringy,Type,Tuned) :-
   check_that(Stringy,[break(var),tuned(stringy)],Tuned),
   check_that(Type,[break(var),
                    tuned(
                       forany(
                          [unifies(var),
                           unifies(atom(_)),
                           unifies(string(_))]
                       ))],Tuned),
   stringy_type_with_length_2(Stringy,Type).

stringy_type_with_length_2(Stringy,var)       :- var(Stringy),!.
stringy_type_with_length_2(Stringy,atom(L))   :- atom(Stringy),!,atom_length(Stringy,L).
stringy_type_with_length_2(Stringy,string(L)) :- string(Stringy),!,string_length(Stringy,L).


