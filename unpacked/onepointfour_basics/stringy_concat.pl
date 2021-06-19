:- module(onepointfour_basics_stringy_concat,
          [
            stringy_concat/3         % stringy_concat(ListOfStringys,?Result,+ResultType)
           ,stringy_concat/4         % stringy_concat(ListOfStringys,?Result,+ResultType,@Tuned)
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

/** <module> Simply concatenate stringys

## Homepage for this code

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_concat.md

*/

%! stringy_concat(ListOfStringy,?Result,?ResultType)
%
% Succeeds if Result is the concatenation of the elements of
% ListOfStringy, coerced to ResultType (one of 'string' or 'atom').
% If Result is nonvar, the ResultType is unified to the actual
% type of Result, so no need to determine it first.

stringy_concat(ListOfStringy,Result,ResultType) :-
   stringy_concat(ListOfStringy,Result,ResultType,soft).

%! stringy_concat(ListOfStringy,?Result,?ResultType,@Tuned)
%
% As stringy_concat/3, but you can make the call throw
% instead of just fail if Result and ResultType are out-of-type
% or out-of-domain, by passing =|hard|= for Tuned.

stringy_concat(ListOfStringy,Result,ResultType,Tuned) :-
   check_that(Result,[break(var),tuned(stringy)],Tuned),
   check_that(ResultType,[break(var),tuned(stringy_typeid)],Tuned),
   check_that(ListOfStringy,hard(proper_list)), % this may be costly
   var_tag(Result,TaggedResult),
   var_tag(ResultType,TaggedResultType),
   instantiate_stringy_type(TaggedResult,TaggedResultType), % fails if Result and ResultType are incompatible
   var_tag(ResultType,RetaggedResultType),                  % ResultType may have been instantiated, so retag
   stringy_concat_2(TaggedResult,RetaggedResultType,ListOfStringy).

stringy_concat_2(var(Result),var(ResultType),ListOfStringy) :-
   !,
   stringy_concat_over_list(ListOfStringy,"",TmpResult),
   (ResultType=atom;ResultType=string),                     % yields two possibilites
   convert_maybe(ResultType,TmpResult,Result).
stringy_concat_2(var(Result),nonvar(ResultType),ListOfStringy) :-
   !,
   stringy_concat_over_list(ListOfStringy,"",TmpResult),
   convert_maybe(ResultType,TmpResult,Result).
stringy_concat_2(nonvar(Result),nonvar(ResultType),ListOfStringy) :-
   !,
   quick_length_check(ListOfStringy,Result),               % may fail due to length mismatch, in which case no need to continue
   stringy_concat_over_list(ListOfStringy,"",TmpResult),
   convert_maybe(ResultType,TmpResult,Result).

stringy_concat_over_list([Stringy|More],RunningString,FinalString) :-
   check_that(Stringy,hard(stringy)),
   string_concat(RunningString,Stringy,NewRunningString),
   stringy_concat_over_list(More,NewRunningString,FinalString).
stringy_concat_over_list([],String,String).

% this code is also used in space_string.pl

instantiate_stringy_type(var(_Stringy),nonvar(_StringyType)) :- !.                   % Do nothing, decision on type to generate has been provided
instantiate_stringy_type(var(_Stringy),var(_StringyType))    :- !.                   % Do nothing, leaving indeterminism on StringyType
instantiate_stringy_type(nonvar(Stringy),var(atom))          :- atom(Stringy),!.     % Instantiate type inside var/1 tag to 'atom'
instantiate_stringy_type(nonvar(Stringy),var(string))        :- string(Stringy),!.   % Instantiate type inside var/1 tag to 'string'
instantiate_stringy_type(nonvar(Stringy),nonvar(atom))       :- atom(Stringy),!.     % Accept only if type is 'atom'
instantiate_stringy_type(nonvar(Stringy),nonvar(string))     :- string(Stringy).     % Accept only if type is 'string'

var_tag(X,var(X))    :- var(X),!.
var_tag(X,nonvar(X)).

% Quick length testing in case "Result" was already instantiated

quick_length_check(ListOfStringy,Result) :-
   stringy_length(Result,MaxLength),
   quick_length_check_2(ListOfStringy,0,MaxLength).

quick_length_check_2([S|More],RunningLength,MaxLength) :-
  stringy_length(S,AddLength), % will throw on bad S
  NewRunningLength is RunningLength + AddLength,
  NewRunningLength =< MaxLength,
  quick_length_check_2(More,NewRunningLength,MaxLength).

quick_length_check_2([],Length,Length). % length must match at the end

% convert_maybe(ResultType,In,Out)
%
% Is called at the end of concatenation to transform the
% concatenated stringy into the string of the desired ResultType.
% Out is actually the Result passed in and may be instantiated
% to a unifiable or non-unifiable stringy.

convert_maybe(atom,In,Out)    :- atom(In)  ,!,In=Out.
convert_maybe(atom,In,Out)    :- string(In),!,atom_string(Out,In).
convert_maybe(string,In,Out)  :- atom(In)  ,!,atom_string(In,Out).
convert_maybe(string,In,Out)  :- string(In),!,In=Out.

