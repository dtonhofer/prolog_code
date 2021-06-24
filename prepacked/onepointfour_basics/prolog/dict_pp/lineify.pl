:- module(onepointfour_dict_pp_lineify,
          [
           lineify/4 % lineify(+Entries,+SettingsDict,?LinesTip,?FinalFin)
          ]).

:- use_module(library('onepointfour_basics/checks.pl')).
:- use_module(library('onepointfour_basics/stringy_concat.pl')).
:- use_module(library('onepointfour_basics/space_stringy.pl')).
:- use_module(library('onepointfour_basics/dict_settings.pl')).
:- use_module(library('onepointfour_basics/stringy_justify.pl')).
:- use_module(library('onepointfour_basics/meta_helpers.pl')).

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

Transforming a list of entries (key-value pairs) generated from a dict, "Entries", 
into a list of strings (nearly) ready for output and appending them to an open
difference list. 

The homepage for this module is at

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_dict_pp.md

*/

%! lineify(+Entries,+SettingsDict,?LinesTip,?FinalFin)
%
% Given the list of entries Entries, which is a list of pairs =|-(KeyString,Lineified)|=,
% and an "open difference list" =|LinesTip-FinalFin|= as receiver, generate a string (a "line")
% for each entry in Entries and add it to the growing list rooted at LinesTip. The "fin2 of 
% the open difference list will be unified with FinalFin.
%
% All we do here is concatenate keys (strings), separators (strings) and
% values (either strings or lists of strings) into lines (a list of strings).
% 
% A diagram for orientation. The =|||= are not in the result, they have been
% added for legibility. Note that a line from a ``poly/1`` compound term
% may be longer than =|MaxMonoWidth|= as it may contain arbitrary results of
% prettyprinting subdicts.
%
% ```
%            keys                              values 
%   |<---- MaxKeyWidth --->|<--Sep-->|<-----MaxMonoWidth---->|
%   |KKKKKKKKKK            |    :    |VVVV                   |    from mono("VVVV")
%   |KKKKKKKKKKKKKKKKKKKKKK|    :    |VVVVVVV                |    from mono("VVVVVVV")
%   |KKKKKK                |    :    |VVVVVVVVVVV            |    from mono("VVVVVVVVVVV")    
%   |KKKKKKKKKK            |    :    |poly#1PPPPPPPPPP       |    from poly([str,str,str])
%   |                      |         |poly#2PPPPPPPPPPPPPPPPPPPPPPPPP
%   |                      |         |poly#3PPPPPPPPPPPPPPPPPPPP
%   |KKKKKKKKKKKKKKK       |    :    |VVVVVVVVVVVVVVVVVVVVVVV|    from mono("VVVVVVVVVVVVVVVVVVVVVVV")
%
% ```

lineify(Entries,SettingsDict,LinesTip,FinalFin) :-
   max_key_width(Entries,MaxKeyWidth),
   max_mono_width(Entries,MaxMonoWidth),
   lineify_entries(
      Entries,
      _{max_key_width:MaxKeyWidth,
        max_mono_width:MaxMonoWidth,
        settings_dict:SettingsDict},  % use a parameter dict for readability
      LinesTip,FinalFin).

% lineify_entries(+Entries,+MaxKeyWidth,+MaxMonoWidth,+SettingsDict,?LinesTip,?FinalFin).
%
% A loop over the list of entries Entries.
%
% Depending on whether the head entry is a pair with a mono(String) or poly(Lines)
% on second place, one or more than one lines may be added to the LinesTip-FinalFin
% open difference list.

% Base case of the empty "Entries" list. LinesTip short-circuits to FinalFin
% (empty open difference list).

lineify_entries([],_,TipIsFin,TipIsFin).

% Case of a pair with a mono(String) on second place.

lineify_entries([KeyString-mono(MonoString)|MoreEntries],Params,[Line|MoreLines],FinalFin) :-
   !,
   justify_key(Params.settings_dict,KeyString,Params.max_key_width,KeyStringOut),
   justify_mono(Params.settings_dict,MonoString,Params.max_mono_width,MonoStringOut),
   separator(Sep),
   stringy_concat([KeyStringOut,Sep,MonoStringOut],Line,string),
   lineify_entries(MoreEntries,Params,MoreLines,FinalFin).

% Case where "Lineified" contains multiple lines ("poly") but the count of lines is actually 0.
% In that case, only the relevant key and a separator are printed.

lineify_entries([KeyString-poly([])|MoreEntries],Params,[Line|MoreLines],FinalFin) :-
   !,
   poly_first_line(KeyString,"",Params,Line),
   lineify_entries(MoreEntries,Params,MoreLines,FinalFin).

% Case where "Lineified" contains multiple lines ("poly") and the count of lines is at least 1.
% They are all appended to the open difference list.

lineify_entries([KeyString-poly([Poly|MorePoly])|MoreEntries],Params,Lines,FinalFin) :-
   filler_string(Params.max_key_width,Filler),
   poly_first_line(KeyString,Poly,Params,FirstLine),
   Lines=[FirstLine|Fin1], % could also be put into the header
   maplist_onto_open_list(
      ({Filler}/[PolyX,Concatted]>>stringy_concat([Filler,PolyX],Concatted,string)),
      MorePoly,
      Fin1,
      Fin2),
   lineify_entries(MoreEntries,Params,Fin2,FinalFin).

% Find the max key width in "Entries" which is a list "KeyString-Lineified"

max_key_width(Entries,Max) :-
   foldl(foldl_mkw,Entries,0,Max).

foldl_mkw(String-_,FromLeft,ToRight) :-
   string_length(String,Width),
   ToRight is max(FromLeft,Width).

% Find the maximum width of any string appearing in a mono(String) value of an entry
% i.e. the entry looks like "KeyString-mono(String)"

max_mono_width(Entries,Max) :-
   foldl(foldl_mmw,Entries,0,Max).

foldl_mmw(_-mono(String),FromLeft,ToRight) :-
   !,
   string_length(String,Width),
   ToRight is max(FromLeft,Width).  % maxify in case of mono(_)

foldl_mmw(_-poly(_),PassThrough,PassThrough). % disregard in case of poly(_)

% Construct the first line for a "poly" case

poly_first_line(KeyString,ValueSideString,Params,LineOut) :-
   justify_key(Params.settings_dict,KeyString,Params.max_key_width,K),
   separator(Sep),
   stringy_concat([K,Sep,ValueSideString],LineOut,string).

% Justify a "mono" value (which is string)

% TODO: After justification, StringOut may have trailing whitespace due to
% both a mix of trailing whitespace present in StringIn (ok) and whitespace
% of the "whitespace string" of width "Width" which is created for justification.
% One may not always want the latter.

justify_mono(SettingsDict,StringIn,Width,StringOut) :-
   get_setting(SettingsDict,justify_value,How,left),
   justify_how(How,Width,StringIn,StringOut,string).

% Justify a key (which is a string) inside a field of width Width

justify_key(SettingsDict,StringIn,Width,StringOut) :-
   get_setting(SettingsDict,justify_key,How,left),
   justify_how(How,Width,StringIn,StringOut,string).

% Construct a filler string for a "poly" case beyond the first line.

filler_string(Width,StringOut) :-
   space_stringy(Width,Spaces,string),  
   empty_separator(Sep),
   stringy_concat([Spaces,Sep],StringOut,string).

% The separator separating a key from a value, and the empty separator
% used if the value consists of several lines. Both should have the same width.

separator(" : ").
empty_separator("   ").


