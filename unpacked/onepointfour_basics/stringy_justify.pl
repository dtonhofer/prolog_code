:- module(onepointfour_basics_stringy_justify,
          [
           justify_left/4   % justify_left(FieldWidth,Text,Result,ResultType)
          ,justify_left/5   % justify_left(FieldWidth,Text,Result,ResultType,SettingsDict)
          ,justify_right/4  % justify_right(FieldWidth,Text,Result,ResultType)
          ,justify_right/5  % justify_right(FieldWidth,Text,Result,ResultType,SettingsDict)
          ,justify_center/4 % justify_center(FieldWidth,Text,Result,ResultType)
          ,justify_center/5 % justify_center(FieldWidth,Text,Result,ResultType,SettingsDict)
          ,justify_how/5    % justify_how(How,FieldWidth,Text,Result,ResultType) 
          ,justify_how/6    % justify_how(How,FieldWidth,Text,Result,ResultType,SettingsDict) 
          ]).

:- use_module(library('onepointfour_basics/stringy_overwrite.pl')).
:- use_module(library('onepointfour_basics/dict_settings.pl')).
:- use_module(library('onepointfour_basics/checks.pl')).
:- use_module(library('onepointfour_basics/stringy_length.pl')).
:- use_module(library('onepointfour_basics/space_stringy.pl')).

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

%! justify_how takes the value How (left,right,center) to select among behaviours. 
%
% Default behaviour in all cases.

justify_how(How,FieldWidth,Text,Result,ResultType) :-
   justify_how(How,FieldWidth,Text,Result,ResultType,_{}).
 
%! justify_how takes the value How (left,right,center) to select among behaviours. 
%
%  SettingsDict can contain:
%
%  cut_left   - a boolean, cut the result at left field limit (default true)
%  cut_right  - a boolean, cut the result at right field limit (default true)
%  offset     - an integer, used for "offset on the left" when justifying left

justify_how(How,FieldWidth,Text,Result,ResultType,SettingsDict) :-
   assertion(check_that(How,[hard(member(left,right,center))])),
   (
     (How==left) 
     -> 
     justify_left(FieldWidth,Text,Result,ResultType,SettingsDict)
     ;
     (How==right) 
     -> 
     justify_right(FieldWidth,Text,Result,ResultType,SettingsDict)
     ;
     (How==center)
     ->
     justify_center(FieldWidth,Text,Result,ResultType,SettingsDict)
   ).

% -----------------------------------------------------------------------------------

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

%! justify_left(FieldWidth,Text,Result,ResultType,SettingsDict)
%
% Standard "left justify", everything is default.

justify_left(FieldWidth,Text,Result,ResultType) :-
   justify_left(FieldWidth,Text,Result,ResultType,_{}).
 
%! justify_left(FieldWidth,Text,Result,ResultType,SettingsDict)
%
% Justify Text left inside a field of width FieldWidth, yielding the
% Result of the type given by ResultType (one of =|string|=, =|atom|=).
% ResultType can also be deduced if Result is instantiated on call,
% or else from the given Text.
%
% SettingsDict can contain:
%
% cut_left   - a boolean, cut the result at left field limit (default true)
% cut_right  - a boolean, cut the result at right field limit (default true)
% offset     - an integer, used for "offset on the left" when justifying left

justify_left(FieldWidth,Text,Result,ResultType,SettingsDict) :-
   decaps_cut_flags(SettingsDict,CutLeft,CutRight),
   decaps_offset(SettingsDict,Offset),
   common_entry_checks(FieldWidth,Text,Result,ResultType),
   complete_result_type(Text,Result,ResultType),
   space_stringy(FieldWidth,BgText,string),
   stringy_overwrite(BgText,Text,Offset,CutLeft,CutRight,Result,ResultType).

% -----------------------------------------------------------------------------------

/* Justify right

   case of positive offset

   |************************FieldWidth****************************|
   |-------PadWidth-----------|*********TextWidth******|**Offset**|

   case of negative offset

   |************************FieldWidth****************************|
   |---------------------PadWidth----------------------|*********TextWidth******|
                                                                  |***Offset****|
*/

%! justify_right(FieldWidth,Text,Result,ResultType)
%
% Standard "right justify", everything is default.

justify_right(FieldWidth,Text,Result,ResultType) :-
   justify_right(FieldWidth,Text,Result,ResultType,_{}).

%! justify_right(FieldWidth,Text,Result,ResultType,SettingsDict)
%
% Justify Text right inside a field of width FieldWidth, yielding the
% Result of the type given by ResultType (one of =|string|=, =|atom|=).
% ResultType can also be deduced if Result is instantiated on call,
% or else from the given Text.
%
% SettingsDict can contain:
%
% cut_left
% cut_right
% offset

justify_right(FieldWidth,Text,Result,ResultType,SettingsDict) :-
   decaps_cut_flags(SettingsDict,CutLeft,CutRight),
   decaps_offset(SettingsDict,Offset),
   common_entry_checks(FieldWidth,Text,Result,ResultType),
   complete_result_type(Text,Result,ResultType),
   stringy_length(Text,TextWidth),
   PadWidth is FieldWidth-TextWidth-Offset,
   space_stringy(FieldWidth,BgText,string),
   stringy_overwrite(BgText,Text,PadWidth,CutLeft,CutRight,Result,ResultType).

% -----------------------------------------------------------------------------------

/* Justify center

   case of positive offset

   |********************************************FieldWidth***********************************************|
   |**OffsetLeft**|---PadLeftWidth---|**********TextWidth*********|----PadRightWidth-----|**OffsetRight**|
                  |~~~~~~~~~~~~~~~~~~~~~~~~ReducedFieldWidth~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|

*/

%! justify_center(FieldWidth,Text,Result,ResultType)
%
% Standard "center justify", everything is default.

justify_center(FieldWidth,Text,Result,ResultType) :-
   justify_center(FieldWidth,Text,Result,ResultType,_{}).

%! justify_center(FieldWidth,Text,Result,ResultType,SettingsDict)
%
% Justify Text centrally inside a field of width FieldWidth, yielding the
% Result of the type given by ResultType (one of =|string|=, =|atom|=).
% ResultType can also be deduced if Result is instantiated on call,
% or else from the given Text.
%
% SettingsDict can contain:
%
% cut_left
% cut_right
% offset_left
% offset_right
% prefer       - leftly,rightly : where to prefer shifting the foreground left or
%                right if full central alignment is impossible

justify_center(FieldWidth,Text,Result,ResultType,SettingsDict) :-
   decaps_offset_left_right(SettingsDict,OffsetLeft,OffsetRight),
   decaps_cut_flags(SettingsDict,CutLeft,CutRight),
   decaps_prefer(SettingsDict,Prefer),
   common_entry_checks(FieldWidth,Text,Result,ResultType),
   complete_result_type(Text,Result,ResultType),
   stringy_length(Text,TextWidth),
   ReducedFieldWidth is FieldWidth-OffsetLeft-OffsetRight, % could already be negative
   ((ReducedFieldWidth < 0)
    ->
    deal_with_negative_reduced_field_width(ReducedFieldWidth,FieldWidth,OffsetLeft,OffsetRight,Text,Result,ResultType,CutLeft,CutRight,Prefer)
    ;
    deal_with_pos0_reduced_field_width(ReducedFieldWidth,FieldWidth,OffsetLeft,Text,TextWidth,Result,ResultType,CutLeft,CutRight,Prefer)).

deal_with_negative_reduced_field_width(ReducedFieldWidth,FieldWidth,OffsetLeft,OffsetRight,Text,Result,ResultType,CutLeft,CutRight,Prefer) :-
   Half           is (-ReducedFieldWidth)//2, % round towards 0 (in principle)
   OtherHalf      is (-ReducedFieldWidth) - Half,
   NewOffsetLeft  is OffsetLeft  - Half,
   NewOffsetRight is OffsetRight - OtherHalf,
   assertion(FieldWidth =:= NewOffsetLeft+NewOffsetRight),
   % calls itself now
   justify_center(FieldWidth,Text,Result,ResultType,
      _{offset_left  : NewOffsetLeft,
        offset_right : NewOffsetRight,
        cut_left     : CutLeft,
        cut_right    : CutRight,
        prefer       : Prefer}).

deal_with_pos0_reduced_field_width(ReducedFieldWidth,FieldWidth,OffsetLeft,Text,TextWidth,Result,ResultType,CutLeft,CutRight,Prefer) :-
   odd_even(ReducedFieldWidth,TaggedReducedFieldWidth),
   odd_even(TextWidth,TaggedTextWidth),
   once(start_pos(TaggedReducedFieldWidth,TaggedTextWidth,Prefer,StartPos)),
   AbsoluteStartPos is OffsetLeft + StartPos,
   space_stringy(FieldWidth,BgText,string),
   stringy_overwrite(BgText,Text,AbsoluteStartPos,CutLeft,CutRight,Result,ResultType).

% -----------------------------------------------------------------------------------

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
   assertion(check_that(PassedResultType,hard(stringy_typeid))).   % PassedResultType must be instantiated now!

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

common_entry_checks(FieldWidth,Text,Result,ResultType) :-
   assertion(check_that(FieldWidth, hard(integer), hard(pos0int),     _{name:"FieldWidth"})),
   assertion(check_that(Text,       hard(stringy),                    _{name:"Text"})),
   assertion(check_that(Result,     break(var), hard(stringy),        _{name:"Result"})),
   assertion(check_that(ResultType, break(var), hard(stringy_typeid), _{name:"ResultType"})).

% Get OffsetLeft and OffsetRight out of the SettingsDict (if missing, they are assumed 0)

decaps_offset_left_right(SettingsDict,OffsetLeft,OffsetRight) :-
   get_setting(SettingsDict,offset_left,OffsetLeft,0),
   get_setting(SettingsDict,offset_right,OffsetRight,0),
   assertion(check_that(OffsetLeft,  hard(integer), _{name:"OffsetLeft"})),
   assertion(check_that(OffsetRight, hard(integer), _{name:"OffsetRight"})).

% Get CutLeft and CutRight out of the SettingsDict (if missing, they are assumed true)

decaps_cut_flags(SettingsDict,CutLeft,CutRight) :-
   get_setting(SettingsDict,cut_left,CutLeft,true),
   get_setting(SettingsDict,cut_right,CutRight,true),
   assertion(check_that(CutLeft,  hard(boolean), _{name:"CutLeft"})),
   assertion(check_that(CutRight, hard(boolean), _{name:"CutRight"})).

% Get Offset out of the SettingsDict (if missing, it is assumed 0)

decaps_offset(SettingsDict,Offset) :-
   get_setting(SettingsDict,offset,Offset,0),
   assertion(check_that(Offset, hard(integer), _{name:"Offset"})).

% Get Prefer out of the SettingsDict (if missing, it is assumed leftly)

decaps_prefer(SettingsDict,Prefer) :-
   get_setting(SettingsDict,prefer,Prefer,leftly),
   assertion(check_that(Prefer, hard(member(leftly,rightly)), _{name:"Prefer"})).

