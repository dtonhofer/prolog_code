:- module(onepointfour_dict_pp_string_stuff,
          [
           max_line_width/2                    % max_line_width(+Lines,-MaxLineWidth)
          ,make_tag_line/6                     % make_tag_line(+Tag,+MaxLineWidth,+PadLeft,+PadRight,+SettingsDict,-Result)
          ,make_bordery_lines/5                % make_bordery_lines(+MaxLineWidth,+PadLeft,+PadRight,-HorizontalBorderResult,-BackgroundResult)
          ,make_background_line_for_padding/4  % make_background_line_for_padding(+MaxLineWIdth,+PadLeft,+PadRight,-Result)
          ]).

:- use_module(library('onepointfour_basics/checks.pl')).
:- use_module(library('onepointfour_basics/stringy_concat.pl')).
:- use_module(library('onepointfour_basics/stringy_justify.pl')).
:- use_module(library('onepointfour_basics/space_stringy.pl')).
:- use_module(library('onepointfour_basics/dict_settings.pl')).

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

/** <module> dict prettyprinter helper predicates

This module collects a few very simple and very specialized helper predicates
dealing with strings in the context of dict prettyprinting, which just add noise
to the main module. So they are here, in a separate module.

The homepage for this module is at
https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_dict_pp.md

*/

%! max_line_width(+Lines,-MaxLineWidth)
%
% Find "maximum line width" over all lines (this means find "max string length" as the lines are strings)

max_line_width(Lines,MaxLineWidth) :-
   foldl(foldl_mlw,Lines,0,MaxLineWidth).

foldl_mlw(Line,FromLeft,ToRight) :-
   string_length(Line,LineWidth),
   ToRight is max(FromLeft,LineWidth).

%! make_tag_line(+Tag,+MaxLineWidth,+PadLeft,+PadRight,+SettingsDict,-Result)
%
% Create the TagLineOut (a string) with the dict's tag Tag by justifying the tag
% inside that line according to the values passed. In principle, the length 
% of string Tag =< MaxLineWidth. At call time, Tag _should_ be an atom or an
% integer, but we cannot be sure, so we pump it through format/3 once. Note that
% is likely that both PadLeft and PadRight are 0.
%
% **Explainer for the various values:**
%
% ```
%   |<-----PadLeft--->|<---- MaxLineWidth ---->|<----PadRight----------->|
%   |                 |left_justified_tag      |                         |
%   |                 |     right_justified_tag|                         |
%   |                 |  center_justified_tag  |                         |
%   |full_left_justified_tag                   |                         |
%   |                 |                        | full_right_justified_tag|
%   |                 |    full_center_justified_tag                     |
% ```
% 
% SettingsDict may carry:
%
% justify_tag      : left,right,center , default: center
% justify_tag_full : true, false       , default: true
% 
% @tbd justify_how/4 could be extended to not perform assertion checks internally. Maybe.
% @tbd justify_how/4 generates trailing spaces in "Result", which might be unwanted.

make_tag_line(Tag,MaxLineWidth,PadLeft,PadRight,SettingsDict,Result) :-
   get_setting(SettingsDict,justify_tag,How,center),
   get_setting(SettingsDict,justify_tag_full,Full,true),
   assertion(check_that(How ,[hard(member([left,right,center]))])),
   assertion(check_that(Full,[hard(boolean)])),
   preprocess_tag(Tag,TagString),
   make_tag_line_2(Full,How,TagString,MaxLineWidth,PadLeft,PadRight,Result).

preprocess_tag(Tag,TagString) :-
   atom(Tag)
   ->
   atom_string(Tag,TagString)
   ;
   string(Tag)
   ->
   Tag=TagString
   ;
   integer(Tag)
   ->
   format(string(TagString),"~d",[Tag])
   ;
   format(string(TagString),"~q",[Tag]).

%! make_tag_line_2(Full,How,TagString,MaxLineWidth,PadLeft,PadRight,Result)
%
% Build the actual line containing the (justified) tag.
% Not exported.

make_tag_line_2(true,How,TagString,MaxLineWidth,PadLeft,PadRight,Result) :- 
   FullWidth is PadLeft + MaxLineWidth + PadRight,
   justify_how(How,FullWidth,TagString,Result,string).  % TODO in all cases whitespace over FullWidth, maybe unwanted?

make_tag_line_2(false,How,TagString,MaxLineWidth,PadLeft,_PadRight,Result) :- 
   justify_how(How,MaxLineWidth,TagString,M,string),    % TODO in all cases whitespace over MaxLineWidth, maybe unwanted?
   space_stringy(PadLeft,LS,string),                    % PadLeft may be 0.
   stringy_concat([LS,M],Result,string).                % Don't bother to append a string of "PadRight" spaces. (should we?)

%! make_bordery_lines(+MaxLineWidth,+PadLeft,+PadRight,-HorizontalBorderResult,-BackgroundResult)
% 
% A packaged call to perform make_horizontal_border_line/2 and make_background_line_with_border/2.

make_bordery_lines(MaxLineWidth,PadLeft,PadRight,HorizontalBorderResult,BackgroundResult) :-
   FullWidth is PadLeft + MaxLineWidth + PadRight,
   make_horizontal_border_line(FullWidth,HorizontalBorderResult),  % create "+-----+" line to put at the top and bottom
   make_background_line_with_border(FullWidth,BackgroundResult).   % create "|     |" line to use as background

%! make_horizontal_border_line(+Width,-Result)
%
% Create a "horizontal border line" that looks like "+--------+".
% There are Width dashes surrounded by "+" for the corners. This predicate is only
% called if a border had been requested.
% Not exported.

make_horizontal_border_line(Width,Result) :-
   length(Chars,Width),                          % TODO: same as the space_stringy, we need a dash_stringy!
   maplist(=("-"),Chars),                        % "Chars" is now "Width" dashes
   atomics_to_string(Chars,S),                   % Fuse into a string
   stringy_concat(["+",S,"+"],Result,string).

%! make_background_line_with_border(+Width,-Result)
%
% Create a "background line" that looks like "|      |" with Width whitespace in between
% the surrounding "|". This predicate is only called if a border has been requested.
% Not exported.

make_background_line_with_border(Width,Result) :-
   space_stringy(Width,S,string),                
   stringy_concat(["|",S,"|"],Result,string).

%! make_background_line_for_padding(+MaxLineWidth,+PadLeft,+PadRight,-Result)
%
% Create a "background line" for padding only: it's just whitespace.

make_background_line_for_padding(MaxLineWidth,PadLeft,PadRight,Result) :-
   FullWidth is PadLeft + MaxLineWidth + PadRight,
   space_stringy(FullWidth,Result,string). 

