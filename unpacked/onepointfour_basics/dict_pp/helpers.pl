:- module(onepointfour_basics_dict_pp_helpers,
          [
           get_padding_settings/5         % get_padding_settings(+SettingDict,-PadTop,-PadBottom,-PadLeft,-PadRight)
          ,get_padding_settings_clamped/6 % get_padding_settings_clamped(+DecisionForPadding,+SettingsDict,-PadTop,-PadBottom,-PadLeft,-PadRight)
          ,pairs_to_entries/3             % pairs_to_entries(+Pairs,+SettingsDict,-Entries)
          ]).

:- use_module(library('onepointfour_basics/checks.pl')).
:- use_module(library('onepointfour_basics/dict_settings.pl')).
:- use_module(library('onepointfour_basics/safe_format.pl')).
:- use_module(library('onepointfour_basics/dict_pp/topmost.pl')). % to perform recursive call to pp_if_shallow_enough

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

% ***************************************************
%  Transforming dict pairs into stringified "entries"
% ***************************************************

%! pairs_to_entries(+Pairs,+SettingsDict,-Entries)
%
% Iterates over the pairs Pairs obtained from the dict that shall be printed
% and transforms them into Entries, a list of structures described below.
%
% Entries contains the transformed pairs in the order in which they appear
% in Pairs.
%
% Each element in Entries (an "entry") is a pair =|KeyString-Lineified|= where:
% 
%    - =|KeyString|= is a string generated from the dict key (which is always 
%      an atom or an integer) The caller will want to justify this string when
%      creating the final line of text.
%    - =|Lineified|= is a compound term that is one of:
%       - ``mono(String)`` : =|String|= is a single string generated by formatting 
%         the value; it can be printed "as is" after left/right/center 
%         justification.
%       - ``poly(List)`` : =|List|= is a list of string generated by formatting a 
%         subdict; all the strings in =|List|= must be integrated into the lines
%         generated for the current dict.

pairs_to_entries(Pairs,SettingsDict,Entries) :-
   maplist(mpl_p2e(SettingsDict),Pairs,Entries).

mpl_p2e(SettingsDict,Key-Value,KeyString-Lineified) :-
   stringify_key(Key,SettingsDict,KeyString),        % straightforward
   lineify_value(Value,SettingsDict,Lineified).      % may cause recursion if the value is a dict
 
% String generation for a dict key. No string justification occurs yet.
% "SettingsDict" is currently passed but not yet needed.

stringify_key(Key,_SettingsDict,String) :-
   (atom(Key)
    -> F="~a"
    ;  integer(Key)
    -> F="~d"
    ;  domain_error([atom,integer],Key)), % never happens unless the implementation of dict changes
   format(string(String),F,[Key]).

% String or multi-string generation for a dict value.

% Case of: "Value" is itself a dict.

lineify_value(Value,SettingsDict,poly(Lines)) :-    
   is_dict(Value),
   !,
   get_dict(depth,SettingsDict,Depth),                 % depth (the depth of the recursion) must exist
   DepthP is Depth+1,
   put_dict(depth,SettingsDict,DepthP,SettingsDict2),
   pp_if_shallow_enough(Value,SettingsDict2,Lines).    % **** recursive call ***** (max depth reached is examined therein)

% Cases of: "Value" is not a dict.

lineify_value(Value,SettingsDict,mono(String)) :-
   integer(Value),
   !,
   get_setting(SettingsDict,spec_int,Placeholder,d),
   spec_format(Placeholder,Value,String).

lineify_value(Value,SettingsDict,mono(String)) :-
   float(Value),
   !,
   get_setting(SettingsDict,spec_float,Placeholder,f),
   spec_format(Placeholder,Value,String).
 
lineify_value(Value,_SettingsDict,mono(String)) :-
   string(Value),
   !,
   format(string(String),"~s",[Value]). % Won't have quotes even if there is whitespace inside

lineify_value(Value,_SettingsDict,mono(String)) :-
   atom(Value),
   !,
   format(string(String),"~a",[Value]). % Won't have quotes even if there is whitespace inside

lineify_value(Value,_SettingsDict,mono(String)) :-
   format(string(String),"~q",[Value]). % By default, quote

% Print 'Value' according to 'Placeholder' (as opposed to, "according to
% a hardcoded format string"), yielding 'String'. We are using "safe_format/3"
% in case there is a problem with the format string. Then an exception message
% will appear in the output, but no exception will be thrown.

spec_format(Placeholder,Value,String) :-
   format(string(Format),"~~~s",[Placeholder]),
   safe_format(Format,[Value],String).

% **********
% Handling settings
% **********

%! get_padding_settings(+SettingDict,-PadTop,-PadBottom,-PadLeft,-PadRight)
%
% Get multiple specific values, those for paddings, in one call.
% This predicate also checks that the retrieved values are all integers >= 0.

get_padding_settings(SettingsDict,PadTop,PadBottom,PadLeft,PadRight) :-
   get_setting(SettingsDict,pad_top,    PadTop    ,0),
   get_setting(SettingsDict,pad_bottom, PadBottom ,0),
   get_setting(SettingsDict,pad_left,   PadLeft   ,0),
   get_setting(SettingsDict,pad_right,  PadRight  ,0),
   check_that(PadTop,hard(pos0int)),
   check_that(PadBottom,hard(pos0int)),
   check_that(PadLeft,hard(pos0int)),
   check_that(PadRight,hard(pos0int)).

%! get_padding_settings_clamped(+DecisionForPadding,+SettingsDict,-PadTop,-PadBottom,-PadLeft,-PadRight)
%
% Get the settings for the padding, but clamp them all to 0 if DecisionForPadding is =false=.

get_padding_settings_clamped(false,_,0,0,0,0) :- !.

get_padding_settings_clamped(true,SettingsDict,PadTop,PadBottom,PadLeft,PadRight) :-
   get_padding_settings(SettingsDict,PadTop,PadBottom,PadLeft,PadRight).

