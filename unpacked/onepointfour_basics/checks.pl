:- module(onepointfour_basics_checks,
          [
            check_that/2
           ,check_that/3
           ,check_that/4 
           ,check_that/5 
           ,check_that/6 
           ,check_that/7 
          ]).

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library('onepointfour_basics/dict_settings.pl')).
:- use_module(library('onepointfour_basics/checks/print_error_message.pl')).
:- use_module(library('onepointfour_basics/checks/throwers.pl')).
:- use_module(library('onepointfour_basics/checks/wellformed.pl')).

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

/** <module> A replacement for must/2

check_that/3 and friends: a replacement for the must_be/2 predicate of
Prolog. must_be/2 is used to check preconditions on predicate entry, but
is not very flexible. Can we improve on that?

The homepage for this module is at

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/doc/README_checks.md

*/

% Various ways of calling check_that/N, which we distinguish (and unify into a call to 
% check_that_1) by examining the arguments.

% The default stance for "tuned" is "hard", i.e. check failure will cause exceptions rather than just failure.
% As specific name for the value can be given via the SettingsDict, with key 'name'. This name will be inserted
% into a generated error message, otherwise the messages in exceptions will be relatively generic.
% If Conditions is not a list, it is transformed into a list first.
% Conditions marked "tuned" in the conditions list are influence by the parameter "Tuned":
% - hard means that condition failure leads to an exception
% - soft means that condition failure leads to just failure
% Whether one or the other is preferred depends on the caller's context. 
% The behaviour can be specifically influenced by testing certain subdoamins first, using "break(var)"
% to unconditionally accept var et.c

% 2 arguments
% old school: conditions in a list

check_that(X,[]) :- 
   !,
   check_that_1(X,[],_,_{}).
check_that(X,[C|Cs]) :- 
   !,
   check_that_1(X,[C|Cs],_,_{}). 

% 2 arguments
% new school: conditions not in a list

check_that(X,C1) :-
   !,
   check_that_1(X,[C1],_,_{}).

% 3 arguments
% old school: conditions in a list

check_that(X,[],Tuned) :-
   \+ is_dict(Tuned),
   !,
   check_that_1(X,[],Tuned,_{}).
check_that(X,[C|Cs],Tuned) :-
   \+ is_dict(Tuned),
   !,
   check_that_1(X,[C|Cs],Tuned,_{}).
check_that(X,[],SettingsDict) :-
   is_dict(SettingsDict),
   !, 
   check_that_1(X,[],_,SettingsDict). 
check_that(X,[C|Cs],SettingsDict) :-
   is_dict(SettingsDict),
   !,
   check_that_1(X,[C|Cs],_,SettingsDict).

% 3 arguments, conditions not in a list (ambiguous)

check_that(X,C1,C2) :-
   not_soft_hard(C2),
   \+ is_dict(C2),
   !,
   check_that_1(X,[C1,C2],_,_{}).
check_that(X,C1,Tuned) :-
   soft_hard(Tuned),
   !,
   check_that_1(X,[C1],Tuned,_{}).
check_that(X,C1,SettingsDict) :-
   is_dict(SettingsDict),
   !,
   check_that_1(X,[C1],_,SettingsDict).
check_that(A1,A2,A3) :-
   throw_2(unmatched_call,"no matching check_that/3",check_that(A1,A2,A3)).

% 4 arguments, conditions not in a list (ambiguous)

check_that(X,C1,C2,C3) :-
   not_soft_hard(C3),
   \+ is_dict(C3),
   !,
   check_that_1(X,[C1,C2,C3],_,_{}).
check_that(X,C1,C2,Tuned) :-
   soft_hard(Tuned),
   !,
   check_that_1(X,[C1,C2],Tuned,_{}).
check_that(X,C1,C2,SettingsDict) :-
   is_dict(SettingsDict),
   !,
   check_that_1(X,[C1,C2],_,SettingsDict).
check_that(A1,A2,A3,A4) :-
   throw_2(unmatched_call,"no matching check_that/4",check_that(A1,A2,A3,A4)).

% 5 arguments, conditions not in a list (ambiguous)

check_that(X,C1,C2,C3,C4) :-
   not_soft_hard(C4),
   \+ is_dict(C4),
   !,
   check_that_1(X,[C1,C2,C3,C4],_,_{}).
check_that(X,C1,C2,C3,Tuned) :-
   soft_hard(Tuned),
   !,
   check_that_1(X,[C1,C2,C3],Tuned,_{}).
check_that(X,C1,C2,C3,SettingsDict) :-
   is_dict(SettingsDict),
   !,
   check_that_1(X,[C1,C2,C3],_,SettingsDict).
check_that(A1,A2,A3,A4,A5) :-
   throw_2(unmatched_call,"no matching check_that/5",check_that(A1,A2,A3,A4,A5)).

% 6 arguments, conditions not in a list (ambiguous)

check_that(X,C1,C2,C3,C4,C5) :-
   not_soft_hard(C5),
   \+ is_dict(C5),
   !,
   check_that_1(X,[C1,C2,C3,C4,C5],_,_{}).
check_that(X,C1,C2,C3,C4,Tuned) :-
   soft_hard(Tuned),
   !,
   check_that_1(X,[C1,C2,C3,C4],Tuned,_{}).
check_that(X,C1,C2,C3,C4,SettingsDict) :-
   is_dict(SettingsDict),
   !,
   check_that_1(X,[C1,C2,C3,C4],_,SettingsDict).
check_that(A1,A2,A3,A4,A5,A6) :-
   throw_2(unmatched_call,"no matching check_that/6",check_that(A1,A2,A3,A4,A5,A6)).

% 7 arguments, conditions not in a list (ambiguous)

check_that(X,C1,C2,C3,C4,C5,C6) :-
   not_soft_hard(C6),
   \+ is_dict(C6),
   !,
   check_that_1(X,[C1,C2,C3,C4,C5,C6],_,_{}).
check_that(X,C1,C2,C3,C4,C5,Tuned) :-
   soft_hard(Tuned),
   !,
   check_that_1(X,[C1,C2,C3,C4,C5],Tuned,_{}).
check_that(X,C1,C2,C3,C4,C5,SettingsDict) :-
   is_dict(SettingsDict),
   !,
   check_that_1(X,[C1,C2,C3,C4,C5],_,SettingsDict).
check_that(A1,A2,A3,A4,A5,A6,A7) :-
   throw_2(unmatched_call,"no matching check_that/7",check_that(A1,A2,A3,A4,A5,A6,A7)).

% helpers for the above

get_tuned(SettingsDict,Tuned) :-
   default_tuned(DefaultTuned),
   get_setting(SettingsDict,tuned,Tuned,DefaultTuned),
   (soft_hard(Tuned) 
    ->    
    true
    ;
    throw_2(domain,"Expected either 'hard' or 'soft' as value of key 'tuned' in the settings dict",Tuned)).

not_soft_hard(P) :- P \== soft, P \== hard.
soft_hard(P)     :- P == soft; P == hard.

                     /*********************************************
                      * What lies beneath the exported predicates *
                      *********************************************/

% ---
% Two-step process:
% 1) Verify the syntactic correctness of all conditions.
%    Throw if there is a problems with the syntax.
%    TODO: That should be done during compilation, i.e. using term expansion, because
%    checking the syntax once instead of at every call should really be enough!
% 2) Evaluate the check tagged by the condition..
%
% For the syntax checks, the code tries to tell the caller _why_ there is a problem,
% instead of just "failing" with zero information left, so it's a bit unidiomatic,
% Prolog needs something to handle that more elegantly.
% ---

default_tuned(hard).
wellformed_check.

check_that_1(X,Conditions,Tuned,SettingsDict) :-
   %
   % ***** TODO: a global variable that says whether this should be called or not ****
   % ***** (it makes a *fat* difference in performance, but tells the programmer about errors) ****
   %
   (
      wellformed_check
      ->
      no_var_in_list_or_throw(Conditions),
      wellformed_conds_or_throw(Conditions,X)
      ;
      true
   ),
   tag_var(Tuned,TaggedTunedFromArg),
   tag_tuned_from_dict(SettingsDict,TaggedTunedFromDict),
   determine_tuned(TaggedTunedFromArg,TaggedTunedFromDict,Tuned2),
   !,
   check_that_2(Conditions,X,Tuned2,SettingsDict).

determine_tuned(var(_FromArg) , var(_FromDict) , Default) :- 
   default_tuned(Default).
determine_tuned(var(_FromArg) , nonvar(hard)   , hard).
determine_tuned(var(_FromArg) , nonvar(soft)   , soft).
determine_tuned(var(_FromArg) , nonvar(X)      , Default) :- 
   default_tuned(Default),
   format(user_error,"Unknown 'tuned' value from settings dict: ~q. Choosing default: ~q.~n",[X,Default]).
determine_tuned(nonvar(hard)  , _              , hard).
determine_tuned(nonvar(soft)  , _              , soft).
determine_tuned(nonvar(X)     , _              , Default) :- 
   default_tuned(Default),
   format(user_error,"Unknown 'tuned' value from argument: ~q. Choosing default: ~q.~n",[X,Default]).

tag_var(X,var(X)) :- var(X),!.
tag_var(X,nonvar(X)).

tag_tuned_from_dict(SettingsDict,Tagged) :-
   assertion(is_dict(SettingsDict)),
   (get_dict(tuned,SettingsDict,InDict) 
    -> 
    tag_var(InDict,Tagged)
    ;
    Tagged=var(_)).
   
no_var_in_list_or_throw(Conditions) :-
   no_var_in_list(Conditions)
   ->
   true
   ;
   throw_2(syntax,"unbound variable in conditions list",Conditions).

no_var_in_list([C|More]) :-
   nonvar(C),
   no_var_in_list(More).
no_var_in_list([]).

% ---
% check_that_2(Conditions,TermToCheck,Tuned,SettingsDict)
%
% One we have passed the basic syntactic/well-formedness checks of
% check_that_1/4, we can proceed to evaluate the checks in-order.
% Generally this means unpacking the compound term of the check and
% calling eval/4.
%
% In case we find an unknown check, we throw ISO type error in the
% penultimate clause. If check_that_1/4 indeed performed
% well-formedness checks, this actually can't happen.
%
% The "SettingsDict" may carry:
% - The name of the TermToCheck for insertion into error messages, under 'name'
% ---

check_that_2([Condition|More],X,Tuned,Dict) :-
   assertion(member(Tuned,[hard,soft])),
   exists_cond_or_throw(Condition), % always succeeds if the syntax check was passed
   check_that_3(Condition,X,Tuned,Outcome,Dict), % needs no internal cut
   !, % no need to go back on success
   outcome_branching(Outcome,Condition,More,X,Tuned,Dict). % recurses
check_that_2([],_,_,_).

outcome_branching(break,_,_,_,_,_)           :- !.
outcome_branching(done,_,_,_,_,_)            :- !.
outcome_branching(fail,_,_,_,_,_)            :- !,fail.
outcome_branching(next,_,More,X,Tuned,Dict)  :- !,check_that_2(More,X,Tuned,Dict).
outcome_branching(Outcome,Condition,_,_,_,_) :- throw_2(unknown_outcome,"bug: condition yields unknown outcome",[Condition,Outcome]).

check_that_3(break(Check),X,_Tuned,Outcome,Dict) :-
   eval(Check,X,soft,hard,Dict) % fail for eval, throw for precondition
   ->
   Outcome = break
   ;
   Outcome = next.
check_that_3(smooth(Check),X,_Tuned,Outcome,Dict) :-
   eval(Check,X,soft,soft,Dict)  % fail for eval, fail for precondition
   ->
   Outcome = next
   ;
   Outcome = fail.
check_that_3(soft(Check),X,_Tuned,Outcome,Dict) :-
   eval(Check,X,soft,hard,Dict)  % fail for eval, throw for precondition
   ->
   Outcome = next
   ;
   Outcome = fail.
check_that_3(tuned(Check),X,Tuned,Outcome,Dict) :-
   eval(Check,X,Tuned,hard,Dict)  % tuned for eval, throw for precondition
   ->
   Outcome = next
   ;
   Outcome = fail.
check_that_3(hard(Check),X,_,Outcome,Dict) :-
   eval(Check,X,hard,hard,Dict) % throw for eval, throw for precondition
   ->
   Outcome = next
   ;
   throw_2(hard_check_fails,"a 'hard' check should throw but instead just fails",Check).
check_that_3([],_,_,_,done,_).

% ---
% Special precondition checks: we want to unconditionally throw if X does not have enough
% information for a meaningful answer, i.e. if we encounter an "instantiation error"
% ---

precondition_X_must_be_instantiated(X,Context,Tuned,Dict) :-
   nonvar(X)
   ->
   true
   ;
   throw_or_fail(instantiation,X,Tuned,be("instantiated",Context),Dict).

precondition_X_must_be_list(X,Context,Tuned,Dict) :-
   is_proper_list(X)
   ->
   true
   ;
   throw_or_fail(type,X,Tuned,be("List",Context),Dict).

precondition_X_must_be_dict(X,Context,Tuned,Dict) :-
   is_dict(X)
   ->
   true
   ;
   throw_or_fail(type,X,Tuned,be("Dict",Context),Dict).

% special case precondition: atomic (used to assess dict key)
% TODO: should really be "integer or atom"

precondition_X_must_be_atomic(X,Context,Tuned,Dict) :-
   atomic(X)
   ->
   true
   ;
   throw_or_fail(type,X,Tuned,be("atomic",Context),Dict).

% special case precondition: X must be instantiated enough to positively say whether it is cyclic

precondition_X_must_be_instantiated_enough_to_decide_whether_cyclic(X,Tuned,Dict) :-
   var(X),
   !,
   throw_or_fail(instantiation,X,Tuned,be("nonvar","Can't say anything about cyclic-ness."),Dict).
precondition_X_must_be_instantiated_enough_to_decide_whether_cyclic(X,Tuned,Dict) :-
   \+ground(X),
   acyclic_term(X),
   !, 
   throw_or_fail(instantiation,X,Tuned,be("nonground and cyclic","Can't say anything about cyclic-ness."),Dict).
precondition_X_must_be_instantiated_enough_to_decide_whether_cyclic(_,_,_). 

% ----
% The "type test" for inty terms: accepts an integer
% or a float that represents an integer. Anything else
% causes failure to type error.
% ---

just_an_inty(X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"inty",TP,Dict),
   ((integer(X);inty_float(X))
    ->
    true
    ;
    throw_or_fail(type(int_or_float),X,Tuned,"inty",Dict)).

% ---
% Accept a float that represents an integer
% round(X)=:=X fails for NaN because NaN =/= NaN, so there is no
% need to test for NaN separately.
% ---

inty_float(X) :-
   float(X),
   X =\= -1.0Inf,
   X =\= +1.0Inf,
   round(X)=:=X.

% ---
% This one just exists to make code clearer
% ---

is_proper_list(L) :-
   is_list(L).

% ---
% eval(Check,TermToCheck,Tuned,PreconditionTuned,Dict)
% This predicate needs no internal cuts as a unique decision is taken on arg 1
% ---

eval(true,_,_,_,_).
eval(false,_,_,_,_).
eval(fail,_,_,_,_).

eval(var,X,Tuned,_TP,Dict) :-
   var(X)
   ->
   true
   ;
   throw_or_fail(uninstantiation,X,Tuned,"var",Dict).

eval(nonvar,X,Tuned,_TP,Dict) :-
   nonvar(X)
   ->
   true
   ;
   throw_or_fail(instantiation,X,Tuned,"nonvar",Dict).

eval(ground,X,Tuned,_TP,Dict) :-
   ground(X)
   ->
   true
   ;
   throw_or_fail(domain,X,Tuned,"ground",Dict).

eval(nonground,X,Tuned,_TP,Dict) :-
   \+ground(X)
   ->
   true
   ;
   throw_or_fail(domain,X,Tuned,"nonground",Dict).

eval(atom,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"atom",TP,Dict),
   (atom(X)
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"atom",Dict)).

eval(symbol,X,Tuned,TP,Dict) :-
   eval(atom,X,Tuned,TP,Dict).

eval(atomic,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"atomic",TP,Dict),
   (atomic(X)
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"atomic",Dict)).

eval(constant,X,Tuned,TP,Dict) :-
   eval(atomic,X,Tuned,TP,Dict).

eval(compound,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"compound",TP,Dict),
   (compound(X)
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"compound",Dict)).

eval(boolean,X,Tuned,TP,Dict) :-
   eval(atom,X,Tuned,TP,Dict),
   ((X==true;X==false)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"boolean",Dict)).

eval(pair,X,Tuned,TP,Dict) :-
   eval(compound,X,Tuned,TP,Dict),
   (X = -(_,_)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"pair",Dict)).

eval(string,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"string",TP,Dict),
   (string(X)
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"string",Dict)).

eval(stringy,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"stringy",TP,Dict),
   ((atom(X);string(X))
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"stringy",Dict)).

eval(nonempty_stringy,X,Tuned,TP,Dict) :-
   eval(stringy,X,Tuned,TP,Dict),
   ((X\=='',X\== "")
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"nonempty stringy",Dict)).

eval(char,X,Tuned,TP,Dict) :-
   eval(atom,X,Tuned,TP,Dict),
   % Note that we need to test atom/1 first because atom_length/2 transforms-to-atom!
   % atom_length/2 may be too wasteful to test for a precise length (?)
   (atom_length(X,1)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"char",Dict)).

eval(code,X,Tuned,TP,Dict) :-
   eval(integer,X,Tuned,TP,Dict),
   ((0=<X,X=<0x10FFFF)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"code",Dict)).

eval(chary,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"chary",TP,Dict),
   (integer(X)
    ->
       ((0=<X,X=<0x10FFFF)
        ->
        true
        ;
        throw_or_fail(domain,X,Tuned,"code",Dict))
    ;
    atom(X)
    ->
       (atom_length(X,1)
        ->
        true
        ;
        throw_or_fail(domain,X,Tuned,"char",Dict))
    ;
    throw_or_fail(type,X,Tuned,"chary",Dict)).

eval(number,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"number",TP,Dict),
   (number(X)
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"number",Dict)).

eval(float,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"float",TP,Dict),
   (float(X)
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"float",Dict)).

eval(int,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"integer",TP,Dict),
   (integer(X)
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"integer",Dict)).

eval(integer,X,Tuned,TP,Dict) :-
   eval(int,X,Tuned,TP,Dict).

eval(rational,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"rational",TP,Dict),
   (rational(X)
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"rational",Dict)).

eval(nonint_rational,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"nonint_rational",TP,Dict),
   (rational(X)
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"nonint_rational",Dict)),
   (integer(X)
    ->
    throw_or_fail(domain,X,Tuned,"nonint_rational",Dict)
    ;
    true).

eval(proper_rational,X,Tuned,TP,Dict) :-
   eval(nonint_rational,X,Tuned,TP,Dict).

eval(negnum,X,Tuned,TP,Dict) :-
   eval(number,X,Tuned,TP,Dict),
   ((X < 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"strictly negative number",Dict)).

eval(negnumber,X,Tuned,TP,Dict) :-
   eval(negnum,X,Tuned,TP,Dict).

eval(posnum,X,Tuned,TP,Dict) :-
   eval(number,X,Tuned,TP,Dict),
   ((X > 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"strictly positive number",Dict)).

eval(posnumber,X,Tuned,TP,Dict) :-
   eval(posnum,X,Tuned,TP,Dict).

eval(neg0num,X,Tuned,TP,Dict) :-
   eval(number,X,Tuned,TP,Dict),
   ((X =< 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"number that is =< 0",Dict)).

eval(neg0number,X,Tuned,TP,Dict) :-
   eval(neg0num,X,Tuned,TP,Dict).

eval(pos0num,X,Tuned,TP,Dict) :-
   eval(number,X,Tuned,TP,Dict),
   ((X >= 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"number that is >= 0",Dict)).

eval(pos0number,X,Tuned,TP,Dict) :-
   eval(pos0num,X,Tuned,TP,Dict).

eval(non0num,X,Tuned,TP,Dict) :-
   eval(number,X,Tuned,TP,Dict),
   ((X =\= 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"number that is not 0",Dict)).

eval(non0number,X,Tuned,TP,Dict) :-
   eval(non0num,X,Tuned,TP,Dict).

eval(float_not_nan,X,Tuned,TP,Dict) :-
   eval(float,X,Tuned,TP,Dict),
   ((NaN is nan,X \== NaN) % arithmetic comparison would fail
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"float that is not NaN",Dict)).

eval(float_not_inf,X,Tuned,TP,Dict) :-
   eval(float,X,Tuned,TP,Dict),
   ((X =\= -1.0Inf,X =\= +1.0Inf)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"float that is not positive or negative infinity",Dict)).

eval(float_not_neginf,X,Tuned,TP,Dict) :-
   eval(float,X,Tuned,TP,Dict),
   ((X =\= -1.0Inf)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"float that is not negative infinity",Dict)).

eval(float_not_posinf,X,Tuned,TP,Dict) :-
   eval(float,X,Tuned,TP,Dict),
   ((X =\= +1.0Inf)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"float that is not positive infinity",Dict)).

eval(negint,X,Tuned,TP,Dict) :-
   eval(int,X,Tuned,TP,Dict),
   ((X<0)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"strictly negative integer",Dict)).

eval(negative_integer,X,Tuned,TP,Dict) :-
   eval(negint,X,Tuned,TP,Dict).

eval(posint,X,Tuned,TP,Dict) :-
   eval(int,X,Tuned,TP,Dict),
   ((X>0)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"strictly positive integer",Dict)).

eval(positive_integer,X,Tuned,TP,Dict) :-
   eval(posint,X,Tuned,TP,Dict).

eval(neg0int,X,Tuned,TP,Dict) :-
   eval(int,X,Tuned,TP,Dict),
   ((X =< 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"integer that is =< 0",Dict)).

eval(pos0int,X,Tuned,TP,Dict) :-
   eval(int,X,Tuned,TP,Dict),
   ((X >= 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"integer that is >= 0",Dict)).

eval(nonneg,X,Tuned,TP,Dict) :-
   eval(pos0int,X,Tuned,TP,Dict).

eval(inty,X,Tuned,TP,Dict) :-
   just_an_inty(X,Tuned,TP,Dict).

eval(neginty,X,Tuned,TP,Dict) :-
   eval(inty,X,Tuned,TP,Dict),
   (((integer(X),X<0);(float(X),X<0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"strictly negative inty",Dict)).

eval(posinty,X,Tuned,TP,Dict) :-
   eval(inty,X,Tuned,TP,Dict),
   (((integer(X),X>0);(float(X),X>0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"strictly positive inty",Dict)).

eval(neg0inty,X,Tuned,TP,Dict) :-
   eval(inty,X,Tuned,TP,Dict),
   (((integer(X),X=<0);(float(X),X=<0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"inty that is =< 0",Dict)).

eval(pos0inty,X,Tuned,TP,Dict) :-
   eval(inty,X,Tuned,TP,Dict),
   (((integer(X),X>=0);(float(X),X>=0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"inty that is >= 0",Dict)).

eval(negfloat,X,Tuned,TP,Dict) :-
   eval(float_not_nan,X,Tuned,TP,Dict),
   (X<0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"strictly negative float",Dict)).

eval(posfloat,X,Tuned,TP,Dict) :-
   eval(float_not_nan,X,Tuned,TP,Dict),
   (X>0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"strictly positive float",Dict)).

eval(neg0float,X,Tuned,TP,Dict) :-
   eval(float_not_nan,X,Tuned,TP,Dict),
   (X=<0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"float that is =< 0",Dict)).

eval(pos0float,X,Tuned,TP,Dict) :-
   eval(float_not_nan,X,Tuned,TP,Dict),
   (X>=0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"float that is >= 0",Dict)).

eval(list,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"list",TP,Dict),
   (is_proper_list(X)
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"proper list",Dict)).

eval(proper_list,X,Tuned,TP,Dict) :-
   eval(list,X,Tuned,TP,Dict).

eval(nonempty_list,X,Tuned,TP,Dict) :-
   eval(list,X,Tuned,TP,Dict),
   (X \== []
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"proper nonempty list",Dict)).

eval(dict,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"dict",TP,Dict),
   (is_dict(X)
    ->
    true
    ;
    throw_or_fail(type,X,Tuned,"dict",Dict)).

eval(stringy_typeid,X,Tuned,TP,Dict) :-
   eval(atom,X,Tuned,TP,Dict),
   ((X==atom;X==string)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"stringy_typeid",Dict)).

eval(chary_typeid,X,Tuned,TP,Dict) :-
   eval(atom,X,Tuned,TP,Dict),
   ((X==char;X==code)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"chary_typeid",Dict)).

eval(char_list,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"char_list",TP,Dict),
   precondition_X_must_be_list(X,"char_list",Tuned,Dict), % Dual traversal, but one is in C, so this may be faster than "unifying" to a single traversal.
   forall(member(MX,X),eval(char,MX,Tuned,TP,Dict)). % TODO: Open up to get at the index

eval(chars,X,Tuned,TP,Dict) :-
   eval(char_list,X,Tuned,TP,Dict).

eval(code_list,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"code_list",TP,Dict),
   precondition_X_must_be_list(X,"code_list",Tuned,Dict), % Dual traversal, but one is in C, so this may be faster than "unifying" to a single traversal.
   forall(member(MX,X),eval(code,MX,Tuned,TP,Dict)). % TODO: Open up to get at the index

eval(codes,X,Tuned,TP,Dict) :-
   eval(code_list,X,Tuned,TP,Dict).

eval(chary_list,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"chary_list",TP,Dict),
   precondition_X_must_be_list(X,"chary_list",Tuned,Dict), % Dual traversal, but one is in C, so this may be faster than "unifying" to a single traversal.
   eval(forany([chars,codes]),X,Tuned,TP,Dict).  % TODO: Open up to get at the index and get better error messages
   % Simple, but the error is confusing for "check_that([a,2],hard(chary_list))" for example and does not give the index

eval(charys,X,Tuned,TP,Dict) :-
   eval(chary_list,X,Tuned,TP,Dict).

% These exist so that one does not need to explicity bracket the arguments of member/N, 
% which increases legibility 

eval(member([]),X,Tuned,TP,Dict) :-
   eval_member_with_list([],X,Tuned,TP,Dict).
eval(member([E|Es]),X,Tuned,TP,Dict) :-
   !,
   eval_member_with_list([E|Es],X,Tuned,TP,Dict).
eval(member(E),X,Tuned,TP,Dict) :-
   eval_member_with_list([E],X,Tuned,TP,Dict).
eval(member(E1,E2),X,Tuned,TP,Dict) :-
   eval_member_with_list([E1,E2],X,Tuned,TP,Dict).
eval(member(E1,E2,E3),X,Tuned,TP,Dict) :-
   eval_member_with_list([E1,E2,E3],X,Tuned,TP,Dict).
eval(member(E1,E2,E3,E4),X,Tuned,TP,Dict) :-
   eval_member_with_list([E1,E2,E3,E4],X,Tuned,TP,Dict).
eval(member(E1,E2,E3,E4,E5),X,Tuned,TP,Dict) :-
   eval_member_with_list([E1,E2,E3,E4,E5],X,Tuned,TP,Dict).
eval(member(E1,E2,E3,E4,E5,E6),X,Tuned,TP,Dict) :-
   eval_member_with_list([E1,E2,E3,E4,E5,E6],X,Tuned,TP,Dict).
eval(member(E1,E2,E3,E4,E5,E6,E7),X,Tuned,TP,Dict) :-
   eval_member_with_list([E1,E2,E3,E4,E5,E6,E7],X,Tuned,TP,Dict).
 
eval(dict_has_key(Key),X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"dict-has-key",TP,Dict),
   precondition_X_must_be_dict(X,"dict-has-key",TP,Dict),
   precondition_X_must_be_instantiated(Key,"dict-has-key",TP,Dict),
   precondition_X_must_be_atomic(Key,"dict-has-key",TP,Dict),
   (get_dict(Key,X,_)
    ->
    true
    ;
    throw_or_fail(domain,[dict-X,key-Key],Tuned,"dict-has-key",Dict)). % domain error sounds right here for "key not in dict"

eval(random(Probability),_,Tuned,_,_) :-
   maybe(Probability)  % throws type error on value not in [0.0,1.0]
   ->
   true
   ;
   throw_or_fail_for_case_random(Tuned).

eval(fail(Msg),X,Tuned,_TP,Dict) :-
   throw_or_fail(explicit_fail,X,Tuned,Msg,Dict).

eval(unifies(Z),X,_,_,_) :-
   \+ \+ (Z = X).

eval(acyclic_now,X,Tuned,_,Dict) :-
   acyclic_term(X) % never throws
   ->
   true
   ;
   throw_or_fail(domain,X,Tuned,"acyclic_now",Dict). % is domain right here?

eval(cyclic_now,X,Tuned,_,Dict) :-
   cyclic_term(X) % never throws
   ->
   true
   ;
   throw_or_fail(domain,X,Tuned,"cyclic_now",Dict). % is domain right here?

eval(acyclic_forever,X,Tuned,_,Dict) :-
   (ground(X),acyclic_term(X)) % never throws
   ->
   true
   ;
   throw_or_fail(domain,X,Tuned,"acyclic_forever",Dict). % is domain right here?

eval(cyclic,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated_enough_to_decide_whether_cyclic(X,TP,Dict),
   (cyclic_term(X)
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"cyclic",Dict)). % is domain right here?

eval(stream,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"stream",TP,Dict),
   (atom(X)
    ->
       (is_stream(X)
        ->
        true
        ;
        throw_or_fail(domain,X,Tuned,"atom-naming-a-stream",Dict))
    ;
    atomic(X)
    ->
       (is_stream(X)
        ->
        true
        ;
        throw_or_fail(domain,X,Tuned,"atomic-designating-a-stream",Dict))
    ;
    throw_or_fail(type,X,Tuned,"atom-or-atomic",Dict)).

% ---
% eval with forall/forany/fornone
% ---

eval(forall(ListOfChecks),X,Tuned,TP,Dict) :-
   forall_forall_loop(ListOfChecks,X,Tuned,TP,Dict) % Tuned is passed
   ->
   true
   ;
   throw_or_fail(forall,checks(ListOfChecks)-item(X),Tuned,"all of the checks succeed for the item",Dict).

eval(forany(ListOfChecks),X,Tuned,TP,Dict) :-
   forany_forall_loop(ListOfChecks,X,TP,Dict) % Tuned is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(forany,checks(ListOfChecks)-item(X),Tuned,"at least one of the checks succeeds for the item",Dict).

eval(fornone(ListOfChecks),X,Tuned,TP,Dict) :-
   fornone_forall_loop(ListOfChecks,X,TP,Dict) % Tuned is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(fornone,checks(ListOfChecks)-item(X),Tuned,"none of the checks succeeds for the item",Dict).

% ---
% eval with passall/passany/passnone
% TODO: - We give not precise indicates which X failed the check how (i..e no index)
%       - Also, passany and passnone do not give any information about what went wrong at
%         all as they fail the individual checks and then throw a generic exception
%       Is fixing both of these worth the complexity?
% ---

eval(passall(Check),ListOfX,Tuned,TP,Dict) :-
   passall_forall_loop(Check,ListOfX,Tuned,TP,Dict) % Tuned is passed; thrown exception informs about problem
   ->
   true
   ;
   throw_or_fail(passall,check(Check)-items(ListOfX),Tuned,"all of the items pass the check",Dict).

eval(passany(Check),ListOfX,Tuned,TP,Dict) :-
   passany_forall_loop(Check,ListOfX,TP,Dict) % Tuned is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(passany,check(Check)-items(ListOfX),Tuned,"at least one of the items passes the check",Dict).

eval(passnone(Check),ListOfX,Tuned,TP,Dict) :-
   passnone_forall_loop(Check,ListOfX,TP,Dict) % Tuned is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(passnone,check(Check)-items(ListOfX),Tuned,"none of the items passes the check",Dict).

% ---
% eval for the special case of "membership in explicitly given list"
% ---

eval_member_with_list(ListOfValues,X,Tuned,TP,Dict) :-
   precondition_X_must_be_instantiated(X,"list_membership",TP,Dict),
   precondition_X_must_be_list(ListOfValues,"list_membership",TP,Dict), % predicate name is confusing here: it is not X but ListOfValues which must be a list!
   ((\+ \+ member(X,ListOfValues)) % Use \+ \+ to roll back any accidental bindings
    ->
    true
    ;
    throw_or_fail(domain,X,Tuned,"list_membership",Dict)).

% ---
% More helpers
% ---

forall_forall_loop(ListOfChecks,X,Tuned,TP,Dict) :-
   % TODO: There is no need to do this if the well-formedness check
   % is on, but it must be done either there or here otherwise the forall
   % call will succeed on non-list (similarly, in forany/fornone)
   eval(proper_list,ListOfChecks,hard,hard,Dict),
   forall(                           % success of ListOfChecks is empty
      member(Check,ListOfChecks),
      eval(Check,X,Tuned,TP,Dict)).

forany_forall_loop(ListOfChecks,X,TP,Dict) :-
   eval(proper_list,ListOfChecks,hard,hard,Dict),
   \+forall(                         % failure if ListOfChecks is empty
     member(Check,ListOfChecks),
     \+eval(Check,X,soft,TP,Dict)).  % disable throwing

fornone_forall_loop(ListOfChecks,X,TP,Dict) :-
   eval(proper_list,ListOfChecks,hard,hard,Dict),
   (ListOfChecks == [])              % force failure if ListOfChecks is empty
   ->
   false
   ;
   forall(
      member(Check,ListOfChecks),
      \+eval(Check,X,soft,TP,Dict)). % disable throwing

passall_forall_loop(Check,ListOfX,Tuned,TP,Dict) :-
   % TODO: There is no need to do this if the well-formedness check
   % is on, but it must be done either there or here otherwise the forall
   % call will succeed on non-list
   eval(proper_list,ListOfX,hard,hard,Dict),
   forall(                           % success if ListOfX is empty
      member(X,ListOfX),
      eval(Check,X,Tuned,TP,Dict)).

passany_forall_loop(Check,ListOfX,TP,Dict) :-
   eval(proper_list,ListOfX,hard,hard,Dict),
   \+forall(                         % failure if ListOfX is empty
      member(X,ListOfX),
      \+eval(Check,X,soft,TP,Dict)). % disable throwing

passnone_forall_loop(Check,ListOfX,TP,Dict) :-
   eval(proper_list,ListOfX,hard,hard,Dict),
   (ListOfX == [])                   % force failure if ListOfX is empty
   ->
   false
   ;
   forall(
      member(X,ListOfX),
      \+eval(Check,X,soft,TP,Dict)). % disable throwing


