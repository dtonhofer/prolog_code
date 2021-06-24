:- module(onepointfour_basics_checks_throwers,
          [
           fail_unless_hard/1               % fail_unless_hard(Tuned)
          ,throw_or_fail_for_case_random/1  % throw_or_fail_for_case_random(Tuned)
          ,throw_or_fail/5                  % throw_or_fail(ErrorTerm,Culprit,Tuned,Ness,Dict)
          ,throw_2/3                        % throw(ErrorTerm,Msg,Culprit)
          ]).

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

/* pldoc ==================================================================== */

/** <module> Some helper predicates for checks.pl

The homepage for this module is at

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_checks.md

*/

% Checking the "Tuned" flag for being set to 'hard' or 'soft'

fail_unless_hard(soft) :- !,fail.
fail_unless_hard(hard).

% Predicates which check whether the Tuned flag is set to 'hard', and if so,
% construct an exception throw and then throw. If the Tuned flag is set to
% 'soft', they just fail.
%
% "Ness" is a a free string that expresses what was expected byut not seen
% For example, if "Ness" is "integer", the message will be about missing
% "integer-ness".

throw_or_fail_for_case_random(Tuned) :-
   fail_unless_hard(Tuned),            % if this fails, the call fails (which is what we want)
   throw_2(random,"random failure as decided by a call to maybe/1",_).

% TODO: This is less than perfect. The protocol for passing info is flaky
throw_or_fail(ErrorTerm,Culprit,Tuned,Info,Dict) :-
   fail_unless_hard(Tuned),            % if this fails, the call fails (which is what we want)
   get_setting(Dict,name,Name,""),     % pick Name out of Dict, which, if missing, is replaced by ""
   name_to_string(Name,NameStr), 
   (
      (Info = be(What))
      ->
      format(string(Msg),"~s should be ~q",[NameStr,What])
      ;
      (Info = be(What,Text))
      ->
      format(string(Msg),"~s should be ~q. ~q",[NameStr,What,Text])
      ;
      format(string(Msg),"~s should fulfill ~q-ness",[NameStr,Info])
   ),
   throw_2(ErrorTerm,Msg,Culprit).

% "Lowermost" throwing predicate constructing the exception term itself.
% All the throws are here for easy modification

throw_2(domain(Expected),Msg,Culprit)  :- throw(error(check(domain            , Expected  , Msg , Culprit)  , _)).
throw_2(type(Expected),Msg,Culprit)    :- throw(error(check(type              , Expected  , Msg , Culprit)  , _)).
throw_2(domain,Msg,Culprit)            :- throw(error(check(domain            , _Expected , Msg , Culprit)  , _)).
throw_2(type,Msg,Culprit)              :- throw(error(check(type              , _Expected , Msg , Culprit)  , _)).
throw_2(uninstantiation,Msg,Culprit)   :- throw(error(check(uninstantiation   , _Expected , Msg , Culprit)  , _)). % ISO's "uninstantiation error"
throw_2(instantiation,Msg,Culprit)     :- throw(error(check(instantiation     , _Expected , Msg , Culprit)  , _)). % ISO's "instantiation error"
throw_2(random,Msg,_)                  :- throw(error(check(random            , _Expected , Msg , _Culprit) , _)).
throw_2(explicit_fail,Msg,Culprit)     :- throw(error(check(explicit_fail     , _Expected , Msg , Culprit)  , _)).
throw_2(call,Msg,Culprit)              :- throw(error(check(call              , _Expected , Msg , Culprit)  , _)).
throw_2(hard_check_fails,Msg,Culprit)  :- throw(error(check(hard_check_fails  , _Expected , Msg , Culprit)  , _)).
throw_2(syntax,Msg,Culprit)            :- throw(error(check(syntax            , _Expected , Msg , Culprit)  , _)).
throw_2(unknown_condition,Msg,Culprit) :- throw(error(check(unknown_condition , _Expected , Msg , Culprit)  , _)).

% I'm not sure about whether these are a good idea:

throw_2(passall,Msg,Culprit)           :- throw(error(check(passall          , _Expected , Msg , Culprit) , _)).
throw_2(passany,Msg,Culprit)           :- throw(error(check(passany          , _Expected , Msg , Culprit) , _)).
throw_2(passnone,Msg,Culprit)          :- throw(error(check(passnone         , _Expected , Msg , Culprit) , _)).
throw_2(forall,Msg,Culprit)            :- throw(error(check(forall           , _Expected , Msg , Culprit) , _)).
throw_2(forany,Msg,Culprit)            :- throw(error(check(forany           , _Expected , Msg , Culprit) , _)).
throw_2(fornone,Msg,Culprit)           :- throw(error(check(fornone          , _Expected , Msg , Culprit) , _)).

% Having this at the end of throw_2/3 saves the day when debugging

throw_2(T,M,C) :- 
   format(string(Msg),"Bug! You forgot a throw_2/3 clause in the source for term '~q', message '~q', culprit '~q'",[T,M,C]),
   throw(Msg).

% Map the name of the variable to something sensible

name_to_string(X  , "the value") :- var(X),!.
name_to_string('' , "the value") :- !.
name_to_string("" , "the value") :- !. 
name_to_string(X  , X          ) :- string(X),!.
name_to_string(X  , Str        ) :- atom(X),!,atom_string(X,Str).
name_to_string(X  , Str        ) :- format(string(Str),"~q",[X]).

