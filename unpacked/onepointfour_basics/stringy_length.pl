:- module(onepointfour_basics_stringy_length,
          [
           stringy_length/2
          ,stringy_length/3
          ,stringy_length/4
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

%! stringy_length(+Stringy,?Length)
%
% Determine the length of Stringy, which may be an atom or a string.
%
% This is not really needed a atom_length/2 and string_length/2 work for
% both strings and atoms, but it removes the specificity of calling
% atom_length/2 or string_length/2.
%
% Length may be instantiated to a integer. In that case, the
% predicate verifies the length of Stringy against Length.
%
% This predicates behaves _softly_, i.e. preferentially fails on bad input.

stringy_length(Stringy,Length) :-
   stringy_length(Stringy,Length,soft).

%! stringy_length(+Stringy,?Length,@Tuned)
%
% As stringy_length/2, but setting Tuned to =|hard|= 
% will make the predicate throw if Length is bound but negative.

stringy_length(Stringy,Length,Tuned) :-
   stringy_length(Stringy,Length,_,Tuned).
 
%! stringy_length(+Stringy,?Length,?Type,@Tuned)
%
% As stringy_length/3, but additionally takes the Type of
% the stringy, which is unified with one of =|atom|= or =|string|=.

stringy_length(Stringy,Length,Type,Tuned) :-
   check_that(Stringy,[hard(nonvar),hard(stringy)]),
   check_that(Length,[break(var),hard(integer),tuned(pos0int)],Tuned),
   check_that(Type,[break(var),tuned(stringy_typeid)],Tuned),
   stringy_length_decide(Stringy,Length,Type).

stringy_length_decide(Stringy,Length,atom)   :- atom(Stringy),!,atom_length(Stringy,Length).
stringy_length_decide(Stringy,Length,string) :- string(Stringy),!,string_length(Stringy,Length).

