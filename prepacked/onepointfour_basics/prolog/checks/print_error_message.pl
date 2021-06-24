:- module(onepointfour_basics_checks_print_error_message,
          [
          % there is actually nothing to export
          ]).

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

/** <module> Hooking into the toplevel error printer 

Code in the module hooks into the toplevel error printer (prolog:error_message//1)
in order to properly printing the non-ISO error term

   error(check(_,_,_,_),_) 

thrown from "check" predicates.

The homepage for this module is at

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_checks.md

*/

:- multifile prolog:error_message//1.  % 1-st argument of error term

% Type     : An atom, describing the "type" of the error. We know of the following:
%
%            domain          : the culprit is outside the required domain
%            type            : the culprit is not of the required type
%            uninstantiation : the culprit is already (fully) instantiated
%            instantiation   : the culprit is not instantiated (enough)
%            random          : this is a random error due to the outcome of maybe/1
%            call            : there is no check_that/N matching the call
%
% Expected : A cleartext description or a term describing what was actually expected 
%            but not encountered. Will be printed on a single line after the
%            text "expected: ". If this is a var, the line is suppressed.
%
% Msg      : A cleartext message about what's going on. Will be printed on a 
%            single line after the text "messag: ". If this is a var, the
%            line is suppressed.
%
% Culprit  : The term this is the reason of the error, for example it might be
%            the atom encountered when an integer was expected.
%            A special case is the Culprit being an uninstantiated variable.

prolog:error_message(check(Type,Expected,Msg,Culprit)) -->
    { build_main_text_pair(Type,MainTextPair) },
    [ MainTextPair, nl ],
    lineify_expected(Expected),
    lineify_msg(Msg),
    lineify_culprit(Type,Culprit).
 
% Build a pair TextWithPlaceholders-ListOfParameters for the toplevel printer

build_main_text_pair(Type,MainTextPair) :-
   extended_msg(Type,ExMsg), 
   !,
   transform_to_string(ExMsg,ExMsgStr),
   transform_to_string(Type,TypeStr),
   MainTextPair = 'check failed : \'~s\' error (~s)'-[TypeStr,ExMsgStr].

build_main_text_pair(Type,MainTextPair) :-
   transform_to_string(Type,TypeStr),
   MainTextPair = 'check failed : \'~s\' error'-[TypeStr].

% Define messages for specific error types.
% If none has been defined, a generic one is created in build_main_text_pair/2.

extended_msg(domain,          "the culprit is outside the required domain").
extended_msg(type,            "the culprit is not of the required type").
extended_msg(uninstantiation, "the culprit is already (fully) instantiated").
extended_msg(instantiation,   "the culprit is not instantiated (enough)").
extended_msg(random,          "this is a random error due to the outcome of maybe/1").
extended_msg(call,            "there is no check_that/N matching the actual call").

% Transforming o SWI-Prolog string, avoiding quotes

transform_to_string(X,X) :-
   string(X),
   !.
transform_to_string(X,Str) :-
   atom(X),
   !,
   atom_string(X,Str).
transform_to_string(X,Str) :-
   format(string(Str),"~q",[X]).

% generate the "expected:" line information

lineify_expected(Expected) -->
   { nonvar(Expected), transform_to_string(Expected,ExpectedStr) },
   !,
   [ '   expected  : ~s'-[ExpectedStr], nl ].
lineify_expected(_) --> [].

% generate the "message:" line information

lineify_msg(Msg) -->
   { nonvar(Msg), transform_to_string(Msg,MsgStr) },
   !,
   [ '   message   : ~s'-[MsgStr], nl ].
lineify_msg(_) --> [].

% generate the "culprit:" line information

lineify_culprit(Type,Culprit) -->
   { nonvar(Culprit), Type\==random, transform_to_string(Culprit,CulpritStr) },
   !,
   [ '   culprit   : ~s'-[CulpritStr], nl ].
lineify_culprit(_,_) --> [].

