:- module(onepointfour_basics_stringy_overwrite,
          [
            stringy_overwrite/5              % stringy_overwrite(BgText,FgText,FgPos,Result,ResultType)
           ,stringy_overwrite/7              % stringy_overwrite(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
           ,stringy_overwrite_using_chars/7  % stringy_overwrite_using_chars(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
           ,stringy_overwrite_using_runs/7   % stringy_overwrite_using_runs(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
          ]).

:- use_module(library('onepointfour_basics/checks.pl')).
:- use_module(library('onepointfour_basics/space_stringy.pl')).
:- use_module(library('onepointfour_basics/stringy_concat.pl')).
:- use_module(library('onepointfour_basics/stringy_length.pl')).
:- use_module(library('onepointfour_basics/stringy_morph.pl')).

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

/** <module> Overwrite a a background text BgText with foreground text FgText

## Homepage for this code

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_overwrite.md

## History

   1. 2020-07-XX: First version
   1. 2021-01-19: Review
   1. 2021-01-29: Changes in naming, review overwrite_intro_assertions/7
   1. 2021-05-27: Review
   1. 2021-06-06: Another review to use the (now completed) "checks"
   1. 2021-06-12: All test cases pass
   1. 2021-06-13: Code rearranged

*/

%! stringy_overwrite(+BgText,+FgText,+FgPos,?Result,+ResultType)
% As overwrite/7, but cuts on both ends by default.

stringy_overwrite(BgText,FgText,FgPos,Result,ResultType) :-
   stringy_overwrite_using_runs(BgText,FgText,FgPos,true,true,Result,ResultType).

%! overwrite(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
%
% Succeeds if Result is the outcome of overwriting BgText (a stringy) with
% FgText (a stringy), with FgText placed at position FgPos (relative to BgText)
% and resulting characters at position < 0 dropped if CutLeft is =|true|= and
% resulting characters at position >= length(BgText) dropped if CutRight is
% =|true|=. The Result is an atom if ResultType is =|atom|= and a string if
% ResultType is =|string|=. In case ResultType is uninstantiated and both
% BgText and FgText are of the same type, that type is used for Result.
%
% CutLeft and CutRight must be one of =|true|=, =|false|=.
% ResultType must be one of =|atom|=, =|string|=.

stringy_overwrite(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType) :-
   stringy_overwrite_using_runs(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType).

% Entry verification called by both overwrite_using_chars/7 and
% overwrite_using_runs/7.

stringy_overwrite_entry(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType) :-
   check_that(BgText     , hard(stringy)),
   check_that(FgText     , hard(stringy)),
   check_that(FgPos      , hard(integer)),
   check_that(CutLeft    , hard(boolean)),
   check_that(CutRight   , hard(boolean)),
   check_that(Result     , break(var),hard(stringy)),         % throw if ResultType instantiated but bad type
   check_that(ResultType , break(var),hard(stringy_typeid)),  % throw if ResultType instantiated but not 'atom' or 'string'
   complete_result_type(BgText,FgText,Result,ResultType),     % succeeds with ResultType instantiated, or throws on missing info, or fails
   assertion(nonvar(ResultType)).

%! overwrite_using_chars(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
%
% This implementation uses character-by-character processing and is slow but
% easy to verify for correctness.

stringy_overwrite_using_chars(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType) :-
   stringy_overwrite_entry(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType),
   stringy_charylist_morph(BgText,BgChars,_,chars),
   stringy_charylist_morph(FgText,FgChars,_,chars),
   stringy_length(BgText,BgLen),
   stringy_length(FgText,FgLen),
   PrelimStartPos is min(FgPos,0),
   PrelimEndPos   is max(BgLen,FgPos+FgLen),
   ((CutLeft == true)
    ->
    StartPos = 0
    ;
    StartPos = PrelimStartPos),
   ((CutRight == true)
    ->
    EndPos = BgLen
    ;
    EndPos = PrelimEndPos),
   FgEnd is FgPos+FgLen,
   collect(StartPos,EndPos,FgPos,FgEnd,FgChars,BgChars,BgLen,Tip,FinalFin),
   FinalFin=[], % close open list
   stringy_charylist_morph(Result,Tip,ResultType,_),
   !.

collect(Pos,EndPos,_,_,_,_,_,Fin,Fin) :-
   Pos == EndPos,
   !.

collect(Pos,EndPos,FgPos,FgEnd,FgChars,BgChars,BgLen,Fin,FinalFin) :-
   Pos < EndPos,
   !,
   (
      (FgPos=<Pos, Pos<FgEnd)
      ->
      (Index is Pos-FgPos,nth0(Index,FgChars,Char)) % use "foreground" character if possible
      ;
      (0=<Pos, Pos<BgLen)
      ->
      nth0(Pos,BgChars,Char)                        % otherwise use "background" character
      ;
      Char=' '                                      % otherwise use space as filler
   ),
   Fin=[Char|NewFin],
   PosPP is Pos+1,
   collect(PosPP,EndPos,FgPos,FgEnd,FgChars,BgChars,BgLen,NewFin,FinalFin).

%! overwrite_using_runs(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
%
% This implementation uses run-of-character processing and is a bit hairy to
% verify, but fast.

stringy_overwrite_using_runs(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType) :-
   stringy_overwrite_entry(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType),
   stringy_length(BgText,BgLen),
   stringy_length(FgText,FgLen),
   FgEnd is FgPos+FgLen,
   fg_completely_or_partially_on_positions_below_position0(FgText,FgPos,FgEnd,CutLeft,R1,ResultType),
   filler_between_end_of_fg_and_position0(FgEnd,CutLeft,R1,R2,ResultType),
   bg_visible_between_position0_and_start_of_fg(FgPos,BgLen,BgText,R2,R3,ResultType),
   fg_covering_bg(FgText,FgPos,FgEnd,BgLen,R3,R4,ResultType),
   bg_visible_between_end_of_fg_and_end_of_bg(BgText,FgEnd,BgLen,R4,R5,ResultType),
   filler_between_end_of_bg_and_start_of_fg(FgPos,BgLen,CutRight,R5,R6,ResultType),
   fg_completely_or_partially_on_the_right(FgText,FgPos,FgLen,FgEnd,BgLen,CutRight,R6,Result,ResultType).

fg_completely_or_partially_on_positions_below_position0(FgText,FgPos,FgEnd,CutLeft,Rnew,ResultType) :-
   (CutLeft == true ; 0 =< FgPos)
   ->
   stringy_concat([],Rnew,ResultType)            % do nothing except returning the empty string/atom
   ;
   (Len is min(0,FgEnd)-FgPos,
    sub_atom(FgText,0,Len,_,Run),                % gives a string
    stringy_concat([Run],Rnew,ResultType)).      % gives sth corresponding to"ResultType"

filler_between_end_of_fg_and_position0(FgEnd,CutLeft,Rprev,Rnew,ResultType) :-
   (CutLeft == true ; 0 =< FgEnd)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (Len is -FgEnd,
    space_stringy(Len,Run,ResultType,hard),
    stringy_concat([Rprev,Run],Rnew,ResultType)). % gives sth corresponding to "ResultType"

bg_visible_between_position0_and_start_of_fg(FgPos,BgLen,Bg,Rprev,Rnew,ResultType) :-
   (FgPos =< 0 ; BgLen == 0)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (Len is min(BgLen,FgPos),
    sub_atom(Bg,0,Len,_,Run),                     % gives a string
    stringy_concat([Rprev,Run],Rnew,ResultType)). % gives sth corresponding to "ResultType"

fg_covering_bg(FgText,FgPos,FgEnd,BgLen,Rprev,Rnew,ResultType) :-
   (FgEnd =< 0 ; BgLen =< FgPos)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (StartPos     is max(0,FgPos),
    StartPosInFg is -min(0,FgPos),
    EndPos       is min(BgLen,FgEnd),
    Len          is EndPos-StartPos,
    sub_atom(FgText,StartPosInFg,Len,_,Run),       % gives an atom
    stringy_concat([Rprev,Run],Rnew,ResultType)).  % gives sth corresponding to "ResultType"

bg_visible_between_end_of_fg_and_end_of_bg(BgText,FgEnd,BgLen,Rprev,Rnew,ResultType) :-
   (BgLen =< FgEnd)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (Len is min(BgLen,BgLen-FgEnd),
    StartPos is max(0,FgEnd),
    sub_atom(BgText,StartPos,Len,_,Run),           % gives an atom
    stringy_concat([Rprev,Run],Rnew,ResultType)).  % gives sth corresponding to "ResultType"

filler_between_end_of_bg_and_start_of_fg(FgPos,BgLen,CutRight,Rprev,Rnew,ResultType) :-
   (FgPos =< BgLen ; CutRight == true)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (Len is FgPos-BgLen,
    space_stringy(Len,Run,ResultType,hard),
    stringy_concat([Rprev,Run],Rnew,ResultType)).  % gives sth corresponding to "ResultType"

fg_completely_or_partially_on_the_right(FgText,FgPos,FgLen,FgEnd,BgLen,CutRight,Rprev,Rnew,ResultType) :-
   (FgEnd =< BgLen ; CutRight == true)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (StartPos is max(BgLen,FgPos),
    Len      is FgEnd-StartPos,
    StartPosInFg is FgLen-Len,
    sub_atom(FgText,StartPosInFg,Len,_,Run),    % gives an atom
    stringy_concat([Rprev,Run],Rnew,ResultType)). % gives sth corresponding to "ResultType"

% In case the ResultType (here called PassedResultType) has been left uninstantiated
% by the caller, clamp it to 'atom' or 'string' if both 'FgText' and 'BgText' are
% atoms, respectively strings. Possibly throw, possibly fail.
% Note that we demand to have a positive conclusion as to what type to deliver or
% we complain, unlike in stringy_concat for example, where lack of a positive
% conclusion leads to non-determinacy, with two solutions delivered.

complete_result_type(BgText,FgText,Result,PassedResultType) :-
   has_type(BgText,BgTextType),
   has_type(FgText,FgTextType),
   has_type(Result,ResultType),
   has_type(PassedResultType,PassedResultTypeType),
   assertion(member(BgTextType,[atom,string])),         % has already been check_that-ed
   assertion(member(FgTextType,[atom,string])),         % has already been check_that-ed
   assertion(member(ResultType,[var,string,atom])),     % has already been check_that-ed
   assertion(member(PassedResultTypeType,[var,atom])),  % has already been check_that-ed (also, if atom, it is one of 'atom', 'string')
   complete_result_type_2(BgTextType,FgTextType,ResultType,PassedResultTypeType,PassedResultType), % this throws, or fails or succeeds, with PassedResultType instantiated
   !,                                                       % complete_result_type_2 generates choicepoint we don't want!
   check_that(PassedResultType , hard(stringy_typeid)).   % PassedResultType must be instantiated now!

%                             FgTextType     PassedResultTypeType
%                  BgTextType     |   ResultType   |
%                      |          |       |        |  PassedResultType (maybe var on call, as indicated by the preceding arg)
%                      |          |       |        |        |
complete_result_type_2(atom   , string , var    , var   , _      ) :- error_msg(Msg),check_that(_,hard(nonvar),_{msg:Msg}). % unconditionally throw
complete_result_type_2(string , atom   , var    , var   , _      ) :- error_msg(Msg),check_that(_,hard(nonvar),_{msg:Msg}). % unconditionally throw
complete_result_type_2(atom   , atom   , var    , var   , atom   ).         % Guess: We want PassedResultType (unbound on call) to be the atom 'atom' because the input texts are both atom
complete_result_type_2(string , string , var    , var   , string ).         % Guess: We want PassedResultType (unbound on call) to be the atom 'string' because the input texts are both string
complete_result_type_2( _     , _      , string , var   , string ).         % Set: We definitely want PassedResultType (unbound on call) to be the atom 'string' because the provided Result is a string
complete_result_type_2( _     , _      , atom   , var   , atom   ).         % Set: We definitely want PassedResultType (unbound on call) to be the atom 'atom' because the provided Result is an atom
complete_result_type_2( _     , _      , var    , atom  , _      ).         % OK: the result type is specified (atom on 4th arg) but the Result is var, so anything is ok
complete_result_type_2( _     , _      , string , atom  , atom   ) :- fail. % FAIL: the result is specified and 'string' but the ResultType is provided and is 'atom'
complete_result_type_2( _     , _      , atom   , atom  , string ) :- fail. % FAIL: the result is specified and 'atom'   but the ResultType is provided and is 'string'
complete_result_type_2( _     , _      , string , atom  , string ).         % OK: the result is specified and 'string' and the ResultType is provided and is 'string'
complete_result_type_2( _     , _      , atom   , atom  , atom   ).         % OK: the result is specified and 'atom'   and the ResultType is provided and is 'atom'

has_type(X,var)    :- var(X),!.
has_type(X,atom)   :- atom(X),!.
has_type(X,string) :- string(X),!.

error_msg("If the input texts are of differing type (here, atom and string) and the Result is unbound, then ResultType must be bound").


