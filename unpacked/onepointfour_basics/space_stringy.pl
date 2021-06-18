:- module(onepointfour_basics_space_stringy,
          [
           space_stringy/3        % space_stringy(?N,?Stringy,+StringyType)
          ,space_stringy/4        % space_stringy(?N,?Stringy,?StringyType,@Tuned) 
          ,space_stringy_lax/3    % space_stringy_lax(?N,?Stringy,?StringyType)
          ,space_stringy_smooth/3 % space_stringy_smooth(?N,?Stringy,?StringyType)
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

/** <module> Generate and accept strings consisting only of char 0x20, "SPACE"

This is specific to SWI-Prolog, which distinguishes "string" and "atom" as
two distinct representations of "sequences of characters".

## Homepage for this code

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_space_stringy.md

## History

   1. 2020-07-XX: First version.
   1. 2021-05-27: Full review.
   1. 2021-06-10: Further review.
   1. 2021-06-11: Back up on github.
   1. 2021-06-17: Rewritten to take "StringyType" arg and generally be clearer code
*/

%! space_stringy(?N,?Stringy,+StringyType)
%
% Succeeds iff Stringy is a string of N _SPACE_ characters
% having type StringyType (either 'atom' or 'string').
%
% If both arguments N and Stringy are unbound, generates pairs 
% (N,Stringy) with N monotonically increasing.
%
% - The predicate throws if bound arguments are out-of-type
% - the call fails for N an integer but N < 0 (any other value for Throw)
 
space_stringy(N,Stringy,StringyType) :-
   space_stringy(N,Stringy,StringyType,soft).

%! space_stringy_smooth(?N,?Stringy,?StringyType)
%
% As space_stringy/3 that only fails, never throws even if it
% is given out-of-type or out-of-domain arguments.

space_stringy_smooth(N,Stringy,StringyType) :-
   check_that(Stringy,[break(var),soft(stringy)]),
   check_that(N,[break(var),soft(int),soft(pos0int)]),
   check_that(StringyType,[break(var),soft(stringy_typeid)]),
   space_stringy_2(N,Stringy,StringyType).

%! space_stringy_lax(?N,?Stringy,?StringyType)
%
% A space_stringy/3 that accepts negative N, and just unifies
% Stringy with an empty "" or '' in that case.

space_stringy_lax(N,Stringy,StringyType) :-
   check_that(Stringy,[break(var),hard(stringy)]),
   check_that(N,[break(var),hard(int)]),
   check_that(StringyType,[break(var),hard(stringy_typeid)]),
   ((nonvar(N))
    -> 
    (M is max(N,0))
    ;
    M=N),
   space_stringy_2(M,Stringy,StringyType).

%! space_stringy(?N,?Stringy,?StringyType,@Throw)
%
% As space_stringy/3 but one can request that:
% - the call throws for N < 0 (Throw = true,throw)
% - the call fails for N < 0 (any other value for Throw)
 
space_stringy(N,Stringy,StringyType,Tuned) :-
   check_that(Stringy,[break(var),hard(stringy)]),
   check_that(N,[break(var),hard(int),tuned(pos0int)],Tuned),
   check_that(StringyType,[break(var),hard(stringy_typeid)]),
   space_stringy_2(N,Stringy,StringyType).

% --- what lies beneath ---
 
space_stringy_2(N,Stringy,StringyType) :-
   var_tag(N,TaggedN),
   var_tag(Stringy,TaggedStringy),
   var_tag(StringyType,TaggedStringyType),
   instantiate_stringy_type(TaggedStringy,TaggedStringyType), % fails if Stringy and StringyType are incompatible
   space_stringy_3(TaggedN,TaggedStringy,StringyType). % StringyType may or may not have been further instantiate by previous call

% this code is also used in stringy_concat.pl

instantiate_stringy_type(var(_Stringy),nonvar(_StringyType)) :- !.                   % Do nothing, decision on type to generate has been provided
instantiate_stringy_type(var(_Stringy),var(_StringyType))    :- !.                   % Do nothing, leaving indeterminism on StringyType
instantiate_stringy_type(nonvar(Stringy),var(atom))          :- atom(Stringy),!.     % Instantiate type inside var/1 tag to 'atom' 
instantiate_stringy_type(nonvar(Stringy),var(string))        :- string(Stringy),!.   % Instantiate type inside var/1 tag to 'string'
instantiate_stringy_type(nonvar(Stringy),nonvar(atom))       :- atom(Stringy),!.     % Accept only if type is 'atom'
instantiate_stringy_type(nonvar(Stringy),nonvar(string))     :- string(Stringy).     % Accept only if type is 'string'

var_tag(X,var(X))    :- var(X),!.
var_tag(X,nonvar(X)).

space_stringy_3(var(N),nonvar(Stringy),_) :-
   !,
   atom_string(Stringy,StringyAsStr),                         % makes sure we have a *string representation*
   string_length(StringyAsStr,N), 
   gen_string_of_spaces(N,StringyAsStr).                      % given N, regenerate N-space string for unification

space_stringy_3(nonvar(N),nonvar(Stringy),_) :-
   !,
   atom_string(Stringy,StringyAsStr),                         % makes sure we have a *string representation*
   gen_string_of_spaces(N,StringyAsStr).                      % given N, regenerate N-space string for unification with Stringy2

space_stringy_3(nonvar(N),var(Stringy),StringyType) :-        % argument 3, StringyType, may or may not have been instantiated on call
   !,
   (var(StringyType)                                          % SWI-Prolog 8.3. cannot determine that space_string_4/3 has just one answer with first arg bound
    ->                                                        % so we introduce determinism manually (this is annoying)
     space_stringy_4(StringyType,nonvar(N),var(Stringy))      % call another predicate for easyness-to-read; that predicate is nondeterministic on StringType.
    ;
     (space_stringy_4(StringyType,nonvar(N),var(Stringy)),!)).

space_stringy_3(var(N),var(Stringy),StringyType) :-           % argument 3, StringyType, may or may not have been instantiated on call
   between(0,inf,N),                                          % infinite backtracking on top of
   space_stringy_4(StringyType,nonvar(N),var(Stringy)).       % two possible types if (StringyType is still unbound at this point)


space_stringy_4(atom,nonvar(N),var(Stringy)) :-
   gen_string_of_spaces(N,StringyAsStr),                      % this may fail for bad N but does not throw
   atom_string(Stringy,StringyAsStr).                         % we want an atom, so convert

space_stringy_4(string,nonvar(N),var(Stringy)) :-     
   gen_string_of_spaces(N,Stringy).                           % this may fail for bad N but does not throw

% Generate (possibly long) strings quickly by recursively applying
% string_concat/3 on two strings of half the desired length.
% Add specific cases besides length 0 and 1 for fast generation for
% short strings.

gen_string_of_spaces( 0 ,"")           :- !.
gen_string_of_spaces( 1 ," ")          :- !.
gen_string_of_spaces( 2 ,"  ")         :- !.
gen_string_of_spaces( 3 ,"   ")        :- !.
gen_string_of_spaces( 4 ,"    ")       :- !.
gen_string_of_spaces( 5 ,"     ")      :- !.
gen_string_of_spaces( 6 ,"      ")     :- !.
gen_string_of_spaces( 7 ,"       ")    :- !.
gen_string_of_spaces( 8 ,"        ")   :- !.
gen_string_of_spaces( 9 ,"         ")  :- !.
gen_string_of_spaces(10 ,"          ") :- !.
gen_string_of_spaces(N  ,String) :-
   N >= 0,                                     % fail if N < 0
   integer(N),                                 % divmod throws on non-integer; preclude that
   divmod(N,2,Times,Remainder),
   gen_string_of_spaces(Times,S1),
   string_concat(S1,S1,S2),                    % S2 := 2*S1
   (
      Remainder>0
      ->
      gen_string_of_spaces(Remainder,SR),
      string_concat(S2,SR,String)
      ;
      String = S2
   ).

