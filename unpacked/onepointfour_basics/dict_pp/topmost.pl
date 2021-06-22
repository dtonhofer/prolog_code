:- module(onepointfour_basics_dict_pp_topmost,
          [
          pp_if_shallow_enough/3   % pp_if_shallow_enough(Dict,SettingsDict,LinesOut)
          ]).

:- use_module(library(yall)).            % yall meta predicates (slow)
:- use_module(library(apply)).           % meta-call predicates (slow)
:- use_module(library(apply_macros)).    % rewrite to avoid meta-calling (transforms the above into fast code)

:- use_module(library('onepointfour_basics/checks.pl')).
:- use_module(library('onepointfour_basics/dict_settings.pl')).
:- use_module(library('onepointfour_basics/stringy_overwrite.pl')).
:- use_module(library('onepointfour_basics/meta_helpers.pl')).
:- use_module(library('onepointfour_basics/stringy_and_charylist_type.pl')).
:- use_module(library('onepointfour_basics/dict_pp/decision.pl')).
:- use_module(library('onepointfour_basics/dict_pp/lineify.pl')).
:- use_module(library('onepointfour_basics/dict_pp/helpers.pl')).
:- use_module(library('onepointfour_basics/dict_pp/string_stuff.pl')).

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

/** <module> Recursively called predicate for dict prettyprinting

This module exports the predicate which is called from dict_pp/3 and
eventually from itself if dict prettyprinting needs to prettyprint subdicts.
*/

%! pp_if_shallow_enough(+Dict,+SettingsDict,-LinesOut)
%
% If the limit of the depth-of-recursion has been reached, print the dict as
% a oneliner using format/3 and the ~q placeholder, otherwise proceed with
% dict prettyprinting. This predicate is recursively called if a (sub-)dict is 
% encountered during prettyprinting.

pp_if_shallow_enough(Dict,SettingsDict,LinesOut) :-
   get_setting(SettingsDict,depth_limit,DepthLimit,10),  % Get value of 'depth_limit', which is >= 0
   get_setting(SettingsDict,depth,Depth),                % 'depth' entry must exist
   check_that(DepthLimit,[hard(pos0int)]),
   ((Depth>=DepthLimit) 
    -> 
    (dict_to_string(Dict,Line),LinesOut=[Line])          % Limit reached; print dict as oneliner!
    ;
    dict_pp_inner(Dict,SettingsDict,LinesOut)).          % Otherwise let's go on...

dict_to_string(Dict,Line) :-
   format(string(Line),"~q",Dict).

% Generate lines for the Dict, then possibly pad them with whitespace into
% a rectangle and possibly put an ASCII border around the rectangle
% depending on what SettingsDict says. Whether the dict's tag is printed
% depends on whether it is set and on what SettingsDict says about that.

dict_pp_inner(Dict,SettingsDict,ResultLines) :-
   %
   % "Dict" -> ("Tag", sorted "Pairs")
   % Does NOT fail if Dict is empty. Pairs will just be [].
   %
   dict_pairs(Dict,Tag,Pairs),
   % 
   % Transform each pair "Key-Value" into an entry "KeyString-Compound"
   % where "KeyString" is a suitably formatted string representing the dict key,
   % and "Compound" is a compound term "mono(String)" or "poly(ListOfString)".
   % This may involve recursively calling dict prettyprinting on (sub-)dicts. 
   %
   pairs_to_entries(Pairs,SettingsDict,Entries),
   %
   % Generate a list of strings "Lines" from the "Pairs" according to "SettingsDict".
   %
   lineify(Entries,SettingsDict,Lines,[]), % "Lines-[]" used to be an open difference list, which we close
   %
   % How to build the output
   %
   decision_how_to_display(Tag,SettingsDict,DecisionForTag,DecisionForBorder,DecisionForPadding),
   assertion(check_that(DecisionForPadding,hard(boolean))),
   assertion(check_that(DecisionForTag,hard(boolean))),
   assertion(check_that(DecisionForBorder,hard(boolean))),
   once(dispatch(DecisionForTag,DecisionForBorder,DecisionForPadding,Lines,Tag,SettingsDict,ResultLines)).

% dispatch(DecisionForTag,DecisionForBorder,DecisionForPadding,Lines,Tag,SettingsDict,ResultLines)

dispatch(true  , true  , DecisionForPadding , Lines , Tag , SettingsDict , LinesOut) :- add_with_border_with_tag(Lines,Tag,DecisionForPadding,SettingsDict,LinesOut).
dispatch(false , true  , DecisionForPadding , Lines , _   , SettingsDict , LinesOut) :- add_with_border_without_tag(Lines,DecisionForPadding,SettingsDict,LinesOut).
dispatch(true  , false , true               , Lines , Tag , SettingsDict , LinesOut) :- add_with_tag_with_padding(Lines,Tag,SettingsDict,LinesOut).
dispatch(true  , false , false              , Lines , Tag , SettingsDict , LinesOut) :- add_with_tag_without_padding(Lines,Tag,SettingsDict,LinesOut).
dispatch(false , false , true               , Lines , _   , SettingsDict , LinesOut) :- add_with_padding(Lines,SettingsDict,LinesOut).
dispatch(false , false , false              , Lines , _   , _            , Lines).      % no tag, no border, no padding -> just transfer lines

add_with_border_with_tag(Lines,Tag,DecisionForPadding,SettingsDict,ResultLines) :-
   get_padding_settings_clamped(DecisionForPadding,SettingsDict,PadTop,PadBottom,PadLeft,PadRight), % if padding is off, pad at least 0
   max_line_width([Tag|Lines],MaxLineWidth),
   make_tag_line(Tag,MaxLineWidth,PadLeft,PadRight,SettingsDict,TagLine),
   make_bordery_lines(MaxLineWidth,PadLeft,PadRight,BorderLine,BgLine), 
   ResultLines=[BorderLine|Fin1],
   append_tag_line(TagLine,BgLine,Fin1,Fin2),
   Fin2=[BorderLine|Fin3],
   Pos is 1+PadLeft,
   append_content(Lines,BgLine,Pos,PadTop,PadBottom,Fin3,Fin4),
   Fin4=[BorderLine],
   assertion(
      (Len is 1+PadLeft+MaxLineWidth+PadRight+1, 
      strings_of_given_length(ResultLines,Len))
   ).

add_with_border_without_tag(Lines,DecisionForPadding,SettingsDict,ResultLines) :- 
   get_padding_settings_clamped(DecisionForPadding,SettingsDict,PadTop,PadBottom,PadLeft,PadRight), % if padding is off, pad at least 0
   max_line_width(Lines,MaxLineWidth),
   make_bordery_lines(MaxLineWidth,PadLeft,PadRight,BorderLine,BgLine), 
   ResultLines=[BorderLine|Fin1],
   Pos is 1+PadLeft,
   append_content(Lines,BgLine,Pos,PadTop,PadBottom,Fin1,Fin2),
   Fin2=[BorderLine],
   assertion(
      (Len is 1+PadLeft+MaxLineWidth+PadRight+1, 
      strings_of_given_length(ResultLines,Len))
   ).

add_with_tag_with_padding(Lines,Tag,SettingsDict,ResultLines) :-
   get_padding_settings(SettingsDict,PadTop,PadBottom,PadLeft,PadRight),
   max_line_width([Tag|Lines],MaxLineWidth),
   make_tag_line(Tag,MaxLineWidth,PadLeft,PadRight,SettingsDict,TagLine),
   ResultLines=[TagLine|Fin1],
   make_background_line_for_padding(MaxLineWidth,PadLeft,PadRight,BgLine),
   append_content(Lines,BgLine,PadLeft,PadTop,PadBottom,Fin1,[]),
   assertion(
      (Len is PadLeft+MaxLineWidth+PadRight,
      strings_of_given_length(ResultLines,Len))
   ).

add_with_tag_without_padding(Lines,Tag,SettingsDict,ResultLines) :-
   max_line_width([Tag|Lines],MaxLineWidth),
   make_tag_line(Tag,MaxLineWidth,0,0,SettingsDict,TagLine), % has whitespace on the right though
   ResultLines=[TagLine|Lines]. % variable whitespace on the right

add_with_padding(Lines,SettingsDict,ResultLines) :-
   get_padding_settings(SettingsDict,PadTop,PadBottom,PadLeft,PadRight),
   max_line_width(Lines,MaxLineWidth),
   make_background_line_for_padding(MaxLineWidth,PadLeft,PadRight,BgLine),
   append_content(Lines,BgLine,PadLeft,PadTop,PadBottom,ResultLines,[]),
   assertion(
      (Len is PadLeft+MaxLineWidth+PadRight, 
      strings_of_given_length(ResultLines,Len))
   ).

% "it is a list of strings and all the strings have the given length"
% If List is not a list in the first place, maplist fails (we assume it is not an open list)

strings_of_given_length(List,Length) :-
   maplist({Length}/[X]>>stringy_type_with_length(X,string(Length)),List).

% Append lines underneath the border line underneath the tag

append_content(Lines,BgLine,Pos,PadTop,PadBottom,ResultLines,FinalFin) :-
   pad_around(PadTop,BgLine,ResultLines,Fin1),                          % append PadTop x BgLine
   maplist_onto_open_list(mpl_overwrite(BgLine,Pos),Lines,Fin1,Fin2),   % append content lines 
   pad_around(PadBottom,BgLine,Fin2,FinalFin).                          % append PadBottom x BgLine

% The tag line is created by overwriting:
%
%  xxxxxxxxxxTAGxxxxxxxx   tag line
% |                     |  background line with left and right box borders

append_tag_line(TagLine,BgLine,ResultLines,FinalFin) :-
   maplist_onto_open_list(mpl_overwrite(BgLine,1),[TagLine],ResultLines,FinalFin).

mpl_overwrite(BgLine,Pos,LineIn,Result) :-
   CutLeft=true,
   CutRight=true,
   stringy_overwrite(BgLine,LineIn,Pos,CutLeft,CutRight,Result,string).

%! pad_around(+PadCount,+PadLine,?Tip,?FinalFin)
%
% Simple iteration for "top" or "bottom" padding around list of lines given
% by an open list difference list. The line inserted is PadLine.

pad_around(0,_,FinalFin,FinalFin) :- !.

pad_around(Count,PadLine,[PadLine|NewFin],FinalFin) :-
   assertion(Count>0),
   CountMinus is Count-1,
   pad_around(CountMinus,PadLine,NewFin,FinalFin).

