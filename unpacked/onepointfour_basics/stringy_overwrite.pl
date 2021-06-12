:- module(onepointfour_basics_stringy_overwrite,
          [
            overwrite/7              % overwrite(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
           ,overwrite_using_chars/7  % overwrite_using_chars(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
           ,overwrite_using_runs/7   % overwrite_using_runs(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
          ]).

:- use_module(library('onepointfour_basics/checks.pl')).
:- use_module(library('onepointfour_basics/space_string.pl')).
:- use_module(library('onepointfour_basics/stringy_concat.pl')).

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

Character position 0 corresponds to the first character of BgText.

The FgText is placed at position FgPos "on top of" BgText. FgPos which may be
negative or beyond the end of BgText. Any gap is filled with SPACE (0x20)
characters.

If CutLeft is =|true|=, then any characters at positions less than 0 are 
deleted.

If CutRight is =|true|=, then any characters at positions larger or equal 
than the length of BgText are deleted.

If ResultType is =|atom|= you will get an atom in Result, if it is =|string|= you
will get a string.

There are two version with the same functionality to compare behaviour:

   - overwrite_using_chars/7: Does brute-force character-by-character
     processing and is slow but easy to verify for correctness.
   - overwrite_using_runs/7: Does run-of-character processing and is a bit
     more difficult to verify for correctness, but fast.

## History

   1. 2020-07-XX: First version
   1. 2021-01-19: Review
   1. 2021-01-29: Changes in naming, review overwrite_intro_assertions/7
   1. 2021-05-27: Review
   1. 2021-06-06: Another review to use the (now completed) "checks"
   1. 2021-06-12: All test cases pass

## Homepage for this code

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_overwrite.md

*/

%! overwrite(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
%
% Succeeds if Result is the outcome of overwriting BgText with FgText, with FgText
% placed at position FgPos and resulting characters at position < 0 dropped if
% CutLeft is =|true|= and resulting characters at position >= length(BgText)
% dropped if CutRight is =|true|= and the Result an atom if ResultType is =|atom|=
% and a string if ResultType is =|string|=.
%
% CutLeft and CutRight must be one of =|true|=, =|false|=.
% ResultType must be one of =|atom|=, =|string|=.

overwrite(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType) :-
   overwrite_using_runs(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType).
 
%! overwrite_using_chars(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
%
% This implementation uses character-by-character processing and
% is slow but easy to verify for correctness.
 
overwrite_using_chars(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType) :-
   check_that(BgText     , [hard(stringy)]),
   check_that(FgText     , [hard(stringy)]),
   check_that(FgPos      , [hard(integer)]),
   check_that(CutLeft    , [hard(boolean)]),
   check_that(CutRight   , [hard(boolean)]),
   check_that(ResultType , [hard(stringy_typeid)]),
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

overwrite_using_runs(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType) :-
   check_that(BgText     , [hard(stringy)]),
   check_that(FgText     , [hard(stringy)]),
   check_that(FgPos      , [hard(integer)]),
   check_that(CutLeft    , [hard(boolean)]),
   check_that(CutRight   , [hard(boolean)]),
   (complete_result_type(BgText,FgText,ResultType) -> true ; true), % an optional op
   check_that(ResultType , [break(var),hard(stringy_typeid)]), % but now we demand a valid ResultType
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

complete_result_type(_BgText,_FgText,ResultType) :- nonvar(ResultType),!. % keep as it is
complete_result_type(BgText,FgText,ResultType) :- var(ResultType),string(BgText),string(FgText),ResultType=string. % all input is string, assume string wanted
complete_result_type(BgText,FgText,ResultType) :- var(ResultType),atom(BgText),atom(FgText),ResultType=atom.       % all input is atom, assume atom wanted

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
    space_string(Len,Run,throw),                  % gives a string
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
    sub_atom(BgText,StartPos,Len,_,Run),         % gives an atom
    stringy_concat([Rprev,Run],Rnew,ResultType)).  % gives sth corresponding to "ResultType"

filler_between_end_of_bg_and_start_of_fg(FgPos,BgLen,CutRight,Rprev,Rnew,ResultType) :-
   (FgPos =< BgLen ; CutRight == true)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (Len is FgPos-BgLen,
    space_string(Len,Run,throw),                 % gives a string
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

