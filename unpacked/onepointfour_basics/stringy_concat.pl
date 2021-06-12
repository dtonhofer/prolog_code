:- module(onepointfour_basics_stringy_concat,
          [
            stringy_concat/3         % stringy_concat(ListOfStringys,?Result,+ResultType)
           ,stringy_concat/4         % stringy_concat(ListOfStringys,?Result,+ResultType,@Throw)
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

## See also

 - [string_concat/3](https://eu.swi-prolog.org/pldoc/doc_for?object=string_concat/3)
 - [atomics_to_string/2](https://eu.swi-prolog.org/pldoc/doc_for?object=atomics_to_string/2)
 - [atomics_to_string/3](https://eu.swi-prolog.org/pldoc/doc_for?object=atomics_to_string/3)
 - [atom_concat/3](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_concat/3)
 - [atomic_concat/3](https://eu.swi-prolog.org/pldoc/doc_for?object=atomic_concat/3)
 - [atomic_list_concat/2](https://eu.swi-prolog.org/pldoc/doc_for?object=atomic_list_concat/2)
 - [atomic_list_concat/3](https://eu.swi-prolog.org/pldoc/doc_for?object=atomic_list_concat/3) - performs _intersperse_ or _join_ of list elements to atom, can _split_ 

## History

   1. 2021-06-12: All test cases pass

## Homepage for this code

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_concat.md

## Examples

*/

%! stringy_concat(ListOfStringy,?Result,?ResultType) 
%
% Succeeds if Result is the concatenation of the elements of
% ListOfStringy, coerced to ResultType (one of 'string' or 'atom').
% If Result is nonvar, the ResultType is unified to the actual
% type of Result, so no need to determine it first.

stringy_concat(ListOfStringy,Result,ResultType) :-
   stringy_concat(ListOfStringy,Result,ResultType,false).

%! stringy_concat(ListOfStringy,?Result,?ResultType,@Throw) 
%
% As stringy_concat/3, but you can make the call throw
% instead of just fail if Result and ResultType are out-of-type
% or out-of-domain, by passing =|throw|= or =|true|= for Throw.

stringy_concat(ListOfStringy,Result,ResultType,Throw) :-
   check_that(Result,[break(var),tuned(stringy)],Throw),
   check_that(ResultType,[break(var),tuned(stringy_typeid)],Throw),
   check_that([Result,ResultType],[hard(passany(nonvar))]),
   check_that(ListOfStringy,hard(proper_list)),
   gleichschaltung(Result,ResultType),   % may fail for certain combinations of Result and ResultType
   assertion(nonvar(ResultType)),        % previous call has instantiated ResultType
   % Concatenate in "string space" any maybe convert at the end
   % Is this good policy? Difficult to say w/o performance tests.
   % If result is already instantiated, we can perform a quick fail-check on the length
   (nonvar(Result)
    ->
    quick_length_check(ListOfStringy,Result) % may fail, in which case no need to continue
    ;
    true),
   stringy_concat_2(ListOfStringy,"",TmpResult),
   convert_maybe(ResultType,TmpResult,Result).

stringy_concat_2([Stringy|More],RunningString,FinalString) :-
   check_that(Stringy,hard(stringy)),
   string_concat(RunningString,Stringy,NewRunningString),
   stringy_concat_2(More,NewRunningString,FinalString).

stringy_concat_2([],String,String).

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

% gleichschaltung(Result,ResultType)
%
% Predicate to make sure that "Result" and "ResultType" correspond.
% At least one of "Result" and "ResultType" is nonvar.
% After the call, "ResultType"'s value has been confirmed against
% the actual type of "Result" (atom or string) or has been set
% to reflect the actual type of "Result". Anything out of that
% domain fails.

gleichschaltung(Result,ResultType) :-
   determine_var_tag(Result,ResultIsVar),
   gleichschaltung_2(ResultIsVar,ResultType,Result),
   !.

gleichschaltung_2(var,_ResultType,_Result).                  % Nothing to do, ResultType is nonvar and determines the type of Result
gleichschaltung_2(nonvar,atom,Result)     :- atom(Result).   % "The ResultType must be / must be set to 'atom' if the Result is known to be an atom"
gleichschaltung_2(nonvar,string,Result)   :- string(Result). % "The ResultType must be / must be set to 'string' if the Result is known to be a string"

determine_var_tag(X,var) :- var(X),!.
determine_var_tag(_,nonvar).

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

