:- module(onepointfour_basics_stringy_justify,
          [
           justify_right/5
          ,justify_right/7
          ,justify_left/5
          ,justify_left/7
          ,justify_center/9
          ]).

:- use_module(library('onepointfour_basics/stringy_overwrite.pl')).

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

/** <module> Justify a text left,right or center inside a field of fixed width

## Homepage for this code

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_justify.md

*/

/* Justify right

   case of positive offset

   |************************FieldWidth****************************|
   |-------PadWidth-----------|*********TextWidth******|**Offset**|

   case of negative offset

   |************************FieldWidth****************************|
   |---------------------PadWidth----------------------|*********TextWidth******|
                                                                  |***Offset****|
*/

%! justify_right(FieldWidth,Offset,Text,Result,ResultType)
%
% Justify right, cutting at the field extremities

justify_right(FieldWidth,Offset,Text,Result,ResultType) :-
   justify_right(FieldWidth,Offset,Text,Result,ResultType,true,true).

%! justify_right(FieldWidth,Offset,Text,Result,ResultType,CutLeft,CutRight)
%
% Justify right, and you may choose whether to cut at the field extremities or not

justify_right(FieldWidth,Offset,Text,Result,ResultType,CutLeft,CutRight) :-
   common_entry_checks(FieldWidth,Text,Result,ResultType,CutLeft,CutRight),
   check_that_named(Offset,[hard(integer)],"Offset"),
   complete_result_type(Text,Result,ResultType),
   stringy_length(Text,TextWidth),
   PadWidth is FieldWidth-TextWidth-Offset,
   space_stringy(FieldWidth,BgSpaceText,string),
   overwrite(BgSpaceText,Text,PadWidth,CutLeft,CutRight,Result,ResultType).

/* Justify left

   case of positive offset

   |************************FieldWidth****************************|
   |**Offset**|******TextWidth*********|---------PadWidth---------|
   |~~~~~~~~~ReducedFieldWidth~~~~~~~~~|

   case of positive offset with negative PadWidth

   |************************FieldWidth****************************|
   |**Offset**|***********************TextWidth*********************************|
   |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReducedFieldWidth~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
                                                                  |--PadWidth---|

   case of negative offset with positive PadWdith

              |************************FieldWidth****************************|
   |**Offset**|******TextWidth*********|-----------------PadWidth------------|
   |~~~~~~~~~ReducedFieldWidth~~~~~~~~~|

*/

%! justify_left(FieldWidth,Offset,Text,Result,ResultType)
%
% Justify left, cutting at the field extremities.

justify_left(FieldWidth,Offset,Text,Result,ResultType) :-
   justify_left(FieldWidth,Offset,Text,Result,ResultType,true,true).

%! justify_left(FieldWidth,Offset,Text,Result,ResultType,CutLeft,CutRight)
%
% Justify left, and you may choose to not cut at the field extremities.

justify_left(FieldWidth,Offset,Text,Result,ResultType,CutLeft,CutRight) :-
   common_entry_checks(FieldWidth,Text,Result,ResultType,CutLeft,CutRight),
   check_that_named(Offset,[hard(integer)],"Offset"),
   complete_result_type(Text,Result,ResultType),
   space_stringy(FieldWidth,BgText,string),
   overwrite(BgText,Text,Offset,CutLeft,CutRight,Result,ResultType).

/* Justify center

   case of positive offset

   |********************************************FieldWidth***********************************************|
   |**OffsetLeft**|---PadLeftWidth---|**********TextWidth*********|----PadRightWidth-----|**OffsetRight**|
                  |~~~~~~~~~~~~~~~~~~~~~~~~ReducedFieldWidth~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|

*/

justify_center(FieldWidth,OffsetLeft,OffsetRight,Text,Result,ResultType,CutLeft,CutRight,LeftlyRightly) :-
   common_entry_checks(FieldWidth,Text,Result,ResultType,CutLeft,CutRight),
   check_that_named(OffsetLeft,[hard(integer)],"OffsetLeft"),
   check_that_named(OffsetRight,[hard(integer)],"OffsetRight"),
   check_that_named(LeftlyRightly,[hard(member([leftly,rightly]))],"LeftlyRightly"),
   complete_result_type(Text,Result,ResultType),
   stringy_length(Text,TextWidth),
   ReducedFieldWidth is FieldWidth-OffsetLeft-OffsetRight, % could already be negative
   ((ReducedFieldWidth < 0)
    ->
    deal_with_negative_reduced_field_width(ReducedFieldWidth,FieldWidth,OffsetLeft,OffsetRight,Text,Result,ResultType,CutLeft,CutRight,LeftlyRightly)
    ;
    deal_with_pos0_reduced_field_width(ReducedFieldWidth,FieldWidth,OffsetLeft,Text,TextWidth,Result,ResultType,CutLeft,CutRight,LeftlyRightly)).

deal_with_negative_reduced_field_width(ReducedFieldWidth,FieldWidth,OffsetLeft,OffsetRight,Text,Result,ResultType,CutLeft,CutRight,LeftlyRightly) :-
   Half           is (-ReducedFieldWidth)//2, % round towards 0 (in principle)
   OtherHalf      is (-ReducedFieldWidth) - Half,
   NewOffsetLeft  is OffsetLeft  - Half,
   NewOffsetRight is OffsetRight - OtherHalf,
   assertion(FieldWidth =:= NewOffsetLeft+NewOffsetRight),
   justify_center(FieldWidth,NewOffsetLeft,NewOffsetRight,Text,Result,ResultType,CutLeft,CutRight,LeftlyRightly).

deal_with_pos0_reduced_field_width(ReducedFieldWidth,FieldWidth,OffsetLeft,Text,TextWidth,Result,ResultType,CutLeft,CutRight,LeftlyRightly) :-
   odd_even(ReducedFieldWidth,TaggedReducedFieldWidth),
   odd_even(TextWidth,TaggedTextWidth),
   start_pos(TaggedReducedFieldWidth,TaggedTextWidth,LeftlyRightly,StartPos),
   AbsoluteStartPos is OffsetLeft + StartPos,
   space_stringy(FieldWidth,BgText,string),
   overwrite(BgText,Text,AbsoluteStartPos,CutLeft,CutRight,Result,ResultType).

odd_even(X,odd(X))  :- (X mod 2) =:= 1, !.
odd_even(X,even(X)).

% In two cases the StartPosition is computed as ReducedFieldWidth//2 - TextWidth//2
%
%   oooooooooXiiiiiiiii       ReducedFieldWidth is odd (and has a central character)
%         aaaXbbb             TextWidth is odd (and has a central character)
%
%                             or
%
%   ooooooooooiiiiiiiiii      ReducedFieldWidth is even
%          aaabbb             TextWidth is even
%
% An arbitrary decision is needed if one of the width is even and one is odd_
%
%   ooooooooooiiiiiiiiii      ReducedFieldWidth is even
%          aaaXbbb            rightly behaviour
%         aaaXbbb             leftly  behaviour
%
%   oooooooooXiiiiiiiii       ReducedFieldWidth is odd (and has a central character)
%          aaabbb             rightly behaviour
%         aaabbb              leftly  behaviour

start_pos(odd(RFW)  ,odd(TW)  ,_       ,StartPos) :- StartPos is RFW//2 - TW//2.
start_pos(even(RFW) ,even(TW) ,_       ,StartPos) :- StartPos is RFW//2 - TW//2.
start_pos(odd(RFW)  ,even(TW) ,leftly  ,StartPos) :- StartPos is RFW//2 - TW//2.
start_pos(odd(RFW)  ,even(TW) ,rightly ,StartPos) :- StartPos is RFW//2 - TW//2 + 1.
start_pos(even(RFW) ,odd(TW)  ,leftly  ,StartPos) :- StartPos is RFW//2 - TW//2 - 1.
start_pos(even(RFW) ,odd(TW)  ,rightly ,StartPos) :- StartPos is RFW//2 - TW//2.

% In case the ResultType (here called PassedResultType) has been left uninstantiated
% by the caller, clamp it to 'atom' or 'string', possibly by guessing.
% Note that we demand to have a positive conclusion as to what type to deliver or
% we complain, unlike in stringy_concat for example, where lack of a positive
% conclusion leads to non-determinacy, with two solutions delivered.

complete_result_type(Text,Result,PassedResultType) :-
   has_type(Text,TextType),
   has_type(Result,ResultType),
   has_type(PassedResultType,PassedResultTypeType),
   assertion(member(TextType,[atom,string])),           % has already been check_that-ed
   assertion(member(ResultType,[var,string,atom])),     % has already been check_that-ed
   assertion(member(PassedResultTypeType,[var,atom])),  % has already been check_that-ed (also, if atom, it is one of 'atom', 'string')
   complete_result_type_2(TextType,ResultType,PassedResultTypeType,PassedResultType), % this throws, or fails or succeeds, with PassedResultType instantiated
   !,                                                   % complete_result_type_2 generates choicepoint we don't want!
   check_that(PassedResultType,[hard(stringy_typeid)]). % PassedResultType must be instantiated now!

%                            ResultType
%                    TextType    |  PassedResultTypeType
%                       |        |        |        +-------PassedResultType (may be var on call, as indicated by the preceding arg)
%                       |        |        |        |
complete_result_type_2(atom   , var    , var   , atom   ).         % Guess: We want PassedResultType (unbound on call) to be the atom 'atom' because the input texts are both atom
complete_result_type_2(string , var    , var   , string ).         % Guess: We want PassedResultType (unbound on call) to be the atom 'string' because the input texts are both string
complete_result_type_2( _     , string , var   , string ).         % Set: We definitely want PassedResultType (unbound on call) to be the atom 'string' because the provided Result is a string
complete_result_type_2( _     , atom   , var   , atom   ).         % Set: We definitely want PassedResultType (unbound on call) to be the atom 'atom' because the provided Result is an atom
complete_result_type_2( _     , var    , atom  , _      ).         % OK: the result type is specified (atom on 4th arg) but the Result is var, so anything is ok
complete_result_type_2( _     , string , atom  , atom   ) :- fail. % FAIL: the result is specified and 'string' but the ResultType is provided and is 'atom'
complete_result_type_2( _     , atom   , atom  , string ) :- fail. % FAIL: the result is specified and 'atom'   but the ResultType is provided and is 'string'
complete_result_type_2( _     , string , atom  , string ).         % OK: the result is specified and 'string' and the ResultType is provided and is 'string'
complete_result_type_2( _     , atom   , atom  , atom   ).         % OK: the result is specified and 'atom'   and the ResultType is provided and is 'atom'

has_type(X,var)    :- var(X),!.
has_type(X,atom)   :- atom(X),!.
has_type(X,string) :- string(X).

common_entry_checks(FieldWidth,Text,Result,ResultType,CutLeft,CutRight) :-
   check_that_named(FieldWidth,[hard(integer),hard(pos0int)],"FieldWidth"),
   check_that_named(Text,[hard(stringy)],"Text"),
   check_that_named(Result,[break(var),hard(stringy)],"Result"),
   check_that_named(ResultType,[break(var),hard(stringy_typeid)],"ResultType"),
   check_that_named(CutLeft,[hard(boolean)],"CutLeft"),
   check_that_named(CutRight,[hard(boolean)],"CutRight").

