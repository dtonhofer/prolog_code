:- module(onepointfour_dict_pp_string_stuff,
          [
           max_line_width/2                    % max_line_width(+Lines,-MaxLineWidth)
          ,make_tag_line/6                     % make_tag_line(+Tag,+MaxLineWidth,+PadLeft,+PadRight,+SettingsDict,-TagLineOut)
          ,make_bordery_lines/5                % make_bordery_lines(+MaxLineWidth,+PadLeft,+PadRight,-BorderLineOut,-BackgroundLineOut)
          ,make_background_line_for_padding/4  % make_background_line_for_padding(+MaxLineWIdth,+PadLeft,+PadRight,-LineOut)
          ]).

:- use_module(library('onepointfour_basics/checks.pl')).
:- use_module(library('onepointfour_basics/stringy_concat.pl')).
:- use_module(library('onepointfour_basics/stringy_justify.pl')).
:- use_module(library('onepointfour_basics/space_stringy.pl')).
:- use_module(library('onepointfour_basics/dict_settings.pl')).

/** <module> dict prettyprinter helper predicates

This module collects a few very simple and very specialized helper predicates
which just add noise to the main module. So they are here.

*/

%! max_line_width(+Lines,-MaxLineWidth)
%
% Find "maximum line width" over all lines (this means find "max string length" as the lines are strings)

max_line_width(Lines,MaxLineWidth) :-
   foldl(foldl_mlw,Lines,0,MaxLineWidth).

foldl_mlw(Line,FromLeft,ToRight) :-
   string_length(Line,LineWidth),
   ToRight is max(FromLeft,LineWidth).

%! make_tag_line(+Tag,+MaxLineWidth,+PadLeft,+PadRight,+SettingsDict,-TagLineOut)
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

make_tag_line(Tag,MaxLineWidth,PadLeft,PadRight,SettingsDict,TagLineOut) :-
   get_setting(SettingsDict,justify_tag,How,center),
   get_setting(SettingsDict,justify_tag_full,Full,true),
   assertion(check_that(How ,[hard(member([left,right,center]))])),
   assertion(check_that(Full,[hard(boolean)])),
   preprocess_tag(Tag,TagString),
   make_tag_line_2(Full,How,TagString,MaxLineWidth,PadLeft,PadRight,TagLineOut).

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

% make_tag_line_2(Full,How,TagString,MaxLineWidth,PadLeft,PadRight,TagLineOut)
%
% Build the actual line containing the tag.
% Not exported.

make_tag_line_2(true,How,TagString,MaxLineWidth,PadLeft,PadRight,TagLineOut) :- 
   FullWidth is PadLeft + MaxLineWidth + PadRight,
   justify_how(How,FullWidth,TagString,TagLineOut,string). % TODO in all cases whitespace over FullWidth, maybe unwanted?

make_tag_line_2(false,How,TagString,MaxLineWidth,PadLeft,_PadRight,TagLineOut) :- 
   justify_how(How,MaxLineWidth,TagString,M,string),    % TODO in all cases whitespace over MaxLineWidth, maybe unwanted?
   space_stringy(PadLeft,LS,string),                    % PadLeft may be 0.
   stringy_concat([LS,M],TagLineOut,string).            % Don't bother to append a string of "PadRight" spaces. (should we?)

%! make_bordery_lines(+MaxLineWidth,+PadLeft,+PadRight,-BorderLineOut,-BackgroundLineOut)
% 
% A packaged call for both make_horizontal_border_line/2 and make_background_line_with_border/2
% in one step.

make_bordery_lines(MaxLineWidth,PadLeft,PadRight,BorderLineOut,BackgroundLineOut) :-
   FullWidth is PadLeft + MaxLineWidth + PadRight,
   make_horizontal_border_line(FullWidth,BorderLineOut),           % create "+-----+" line to put at the top and bottom
   make_background_line_with_border(FullWidth,BackgroundLineOut).  % create "|     |" line to use as background

% make_horizontal_border_line(+Width,-LineOut)
%
% Create a "horizontal border line" that looks like "+--------+".
% There are Width dashes surrounded by "+" for the corners. This predicate is only
% called if a border had been requested.
% Not exported.

make_horizontal_border_line(Width,LineOut) :-
   length(Chars,Width),                          % TODO: same as the space_stringy, we need a dash_stringy!
   maplist(=("-"),Chars),                        % "Chars" is now "Width" dashes
   atomics_to_string(Chars,S),                   % Fuse into a string
   stringy_concat(["+",S,"+"],LineOut,string).

% make_background_line_with_border(+Width,-LineOut)
%
% Create a "background line" that looks like "|      |" with Width whitespace in between
% the surrounding "|". This predicate is only called if a border has been requested.
% Not exported.

make_background_line_with_border(Width,LineOut) :-
   space_stringy(Width,S,string),                
   stringy_concat(["|",S,"|"],LineOut,string).

%! make_background_line_for_padding(+MaxLineWidth,+PadLeft,+PadRight,-LineOut)
%
% Create a "background line" for padding only: it's just whitespace.

make_background_line_for_padding(MaxLineWidth,PadLeft,PadRight,LineOut) :-
   FullWidth is PadLeft + MaxLineWidth + PadRight,
   space_stringy(FullWidth,LineOut,string). 

