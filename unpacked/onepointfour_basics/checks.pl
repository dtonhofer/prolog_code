:- module(onepointfour_basics_checks,
          [
           check_that/2        % check_that(X,Conditions)
          ,check_that/3        % check_that(X,Conditions,Tuned)
          ,check_that_named/3  % check_that_named(X,Conditions,Name)
          ,check_that_named/4  % check_that_named(X,Conditions,Name,Tuned)
          ]).

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).


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

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_checks.md

*/

% ---
% 1) For this call, tuned/1 conditions will fail instead of throw (but still throw if the precondition fails)
% 2) No specific name for the value is given, so messages in exceptions will be relatively generic.
% If Conditions is not a list, it is transformed into a list first.
% ---

check_that(X,Conditions) :-
   check_that(X,Conditions,soft).

% ---
% 1) For this call, you can select whether tuned/1 conditions should fail or throw. If throwing is preferred,
%    set Tuned to the atom =|hard|=. Anything else (although we prefer to see =|soft|= for clarity)
%    means to prefer failing.
% 2) No specific name for the value is given, so messages in exceptions will be relatively generic.
% If Conditions is not a list, it is transformed into a list first.
% ---

check_that(X,Conditions,Tuned) :-
   is_proper_list(Conditions)
   ->
   check_that_1(Conditions,X,"",Tuned)
   ;
   check_that_1([Conditions],X,"",Tuned).

% ---
% As above, but additionally pass a name to be be used in messages used in exceptions
% ---

check_that_named(X,Conditions,Name) :-
   check_that_named(X,Conditions,Name,soft).

% ---
% As above, but additionally pass a name to be be used in messages used in exceptions
% ---

check_that_named(X,Conditions,Name,Tuned) :-
   is_proper_list(Conditions)
   ->
   check_that_1(Conditions,X,Name,Tuned)
   ;
   check_that_1([Conditions],X,Name,Tuned).


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

check_that_1(Conditions,X,Name,Tuned) :-
   % syntax check
   %
   % ***** TODO: a gloabl variable that says whether this should be called or not ****
   % ***** (it makes a fat difference in performance, but tells the programmer about errors) ****
   %
   WellFormedCheck=true,
   (
      (WellFormedCheck==true)
      ->
      no_var_in_list_or_throw(Conditions),
      wellformed_conds_or_throw(Conditions,X)
      ;
      true
   ),
   check_that_2(Conditions,X,Name,Tuned).

no_var_in_list_or_throw(Conditions) :-
   no_var_in_list(Conditions)
   ->
   true
   ;
   throw_various(syntax,"unbound variable in conditions list",Conditions).

no_var_in_list([C|More]) :-
   nonvar(C),
   no_var_in_list(More).
no_var_in_list([]).

wellformed_conds_or_throw(Conditions,X) :-
   wellformed_conds(Conditions,X) % may also throw
   ->
   true
   ;
   throw_various(syntax,"conditions do not pass syntax check",Conditions).

% ---
% wellformed_conds(Conditions,X)
%
% Verify the conds of Conditions for syntactic correctness. This is done
% outside of actual check-evaluation eval/4, to have clean code and to be able to
% disable the verification for well-formedness by (currently) commenting out
% the call to wellformed/2.
% ---

wellformed_conds([Condition|More],X) :-
   exists_cond_or_throw(Condition),
   wellformed_cond_or_throw(Condition,X),
   wellformed_conds(More,X).
wellformed_conds([],_).

exists_cond_or_throw(Condition) :-
   exists_cond(Condition)
   ->
   true
   ;
   throw_various(unknown_condition,"unknown condition found during syntax check",Condition).

exists_cond(break(_Check)).
exists_cond(smooth(_Check)).
exists_cond(soft(_Check)).
exists_cond(tuned(_Check)).
exists_cond(hard(_Check)).

wellformed_cond_or_throw(Condition,X) :-
   atom(Condition)
   ->
   true
   ;
   (Condition =.. [_,Check], wellformed_check_or_throw(Check,X)).

wellformed_check_or_throw(Check,X) :-
   wellformed_check_2(Check,X)
   ->
   true
   ;
   throw_various(unknown_or_problematic_check,"unknown or problematic check found during syntax check",Check).

wellformed_check_2(Check,_)                 :- atom(Check),!,atomoform_checks(AFCs),memberchk(Check,AFCs).
wellformed_check_2(member(ListOfValues),_)  :- is_proper_list(ListOfValues).
wellformed_check_2(dict_has_key(_),_).      % just test the form, leave the testing of Key and Dict type to the actual check
wellformed_check_2(type(ListOfTypes),_)     :- is_proper_list(ListOfTypes),atomoform_checks(AFCs),maplist({AFCs}/[T]>>memberchk(T,AFCs),ListOfTypes).
wellformed_check_2(random(Probability),_)   :- number(Probability),0=<Probability,Probability=<1.
wellformed_check_2(unifies(_),_).
wellformed_check_2(forall(ListOfChecks),X)  :- wellformed_list_of_checks(ListOfChecks,X).
wellformed_check_2(forany(ListOfChecks),X)  :- wellformed_list_of_checks(ListOfChecks,X).
wellformed_check_2(fornone(ListOfChecks),X) :- wellformed_list_of_checks(ListOfChecks,X).
wellformed_check_2(passall(Check),ListOfX)  :- wellformed_check_over_list(Check,ListOfX).
wellformed_check_2(passany(Check),ListOfX)  :- wellformed_check_over_list(Check,ListOfX).
wellformed_check_2(passnone(Check),ListOfX) :- wellformed_check_over_list(Check,ListOfX).

wellformed_list_of_checks(ListOfChecks,X) :-
   is_proper_list(ListOfChecks),
   forall(
      member(Check,ListOfChecks),
      wellformed_check_or_throw(Check,X)). % ** recursive **

wellformed_check_over_list(Check,ListOfX) :-
   is_proper_list_or_throw(Check,ListOfX),
   forall(
      member(M,ListOfX),
      wellformed_check_or_throw(Check,M)). % ** recursive **

is_proper_list_or_throw(Check,ListOfX) :-
   is_proper_list(ListOfX)
   ->
   true
   ;
   throw_various(type,"check needs a list as argument",[check(Check),arg(ListOfX)]).

% ---
% All the atoms for "elementary checks", i.e. those that
% are not a compound term, are listed here.
% ---

atomoform_checks(
   [
   var,nonvar,
   nonground,ground,
   atom,symbol,
   atomic,constant,
   compound,
   boolean,
   pair,
   string,stringy,
   char,code,chary,
   char_list,chars,
   code_list,codes,
   chary_list,charys,
   nonempty_stringy,
   stringy_typeid,
   chary_typeid,
   number,float,integer,int,rational,nonint_rational,proper_rational,
   negnum,negnumber,
   posnum,posnumber,
   neg0num,neg0number,
   pos0num,pos0number,
   non0num,non0number,
   float_not_nan,
   float_not_inf,
   float_not_neginf,
   float_not_posinf,
   negint,negative_integer,
   posint,positive_integer,
   neg0int,pos0int,nonneg,
   negfloat,posfloat,
   neg0float,pos0float,
   inty,
   neginty,posinty,
   neg0inty,pos0inty,
   list,proper_list,
   nonempty_list,
   dict,
   cyclic,cyclic_now,acyclic_now,acyclic_forever,
   stream
   ]
).

% ---
% check_that_2(Conditions,TermToCheck,NameOfTerm,Tuned)
%
% One we have passed the basic syntactic/well-formedness checks of
% check_that_1/4, we can proceed to evaluate the checks in-order.
% Generally this means unpacking the compound term of the check and
% calling eval/4.
%
% In case we find an unknown check, we throw ISO type error in the
% penultimate clause. If check_that_1/4 indeed performed
% well-formedness checks, this actually can't happen.
% ---

check_that_2([Condition|More],X,Name,Tuned) :-
   exists_cond_or_throw(Condition), % always succeeds if the syntax check was passed
   check_that_3(Condition,X,Name,Tuned,Outcome), % needs no internal cut
   !, % no need to go back on success
   outcome_branching(Outcome,Condition,More,X,Name,Tuned). % recurses
check_that_2([],_,_,_).

outcome_branching(break,_,_,_,_,_)           :- !.
outcome_branching(done,_,_,_,_,_)            :- !.
outcome_branching(fail,_,_,_,_,_)            :- !,fail.
outcome_branching(next,_,More,X,Name,Tuned)  :- !,check_that_2(More,X,Name,Tuned).
outcome_branching(Outcome,Condition,_,_,_,_) :- throw_various(unknown_outcome,"bug: condition yields unknown outcome",[Condition,Outcome]).

check_that_3(break(Check),X,Name,_Tuned,Outcome) :-
   eval(Check,X,Name,soft,hard) % fail for eval, throw for precondition
   ->
   Outcome = break
   ;
   Outcome = next.

check_that_3(smooth(Check),X,Name,_Tuned,Outcome) :-
   eval(Check,X,Name,soft,soft)  % fail for eval, fail for precondition
   ->
   Outcome = next
   ;
   Outcome = fail.

check_that_3(soft(Check),X,Name,_Tuned,Outcome) :-
   eval(Check,X,Name,soft,hard)  % fail for eval, throw for precondition
   ->
   Outcome = next
   ;
   Outcome = fail.

check_that_3(tuned(Check),X,Name,Tuned,Outcome) :-
   eval(Check,X,Name,Tuned,hard)  % tuned for eval, throw for precondition
   ->
   Outcome = next
   ;
   Outcome = fail.

check_that_3(hard(Check),X,Name,_,Outcome) :-
   eval(Check,X,Name,hard,hard) % throw for eval, throw for precondition
   ->
   Outcome = next
   ;
   throw_various(hard_check_fails,"hard check should throw but instead fails",Check).

check_that_3([],_,_,_,done).

% ---
% Checking the "Tuned" flag for being set
% ---

fail_if_not_hard(Tuned) :- Tuned==hard,!.
fail_if_not_hard(Tuned) :- Tuned==true,!,format(user_error,"The passed Tuned flag is ~q. Preferring 'hard'~q",[Tuned]).
fail_if_not_hard(Tuned) :- Tuned==throw,!,format(user_error,"The passed Tuned flag is ~q. Preferring 'hard'~n",[Tuned]).
fail_if_not_hard(Tuned) :- Tuned==soft,!,fail.
fail_if_not_hard(Tuned) :- format(user_error,"The passed Tuned flag is ~q. Preferring 'soft'~n",[Tuned]),fail.

% ---
% Special precondition checks: we want to unconditionally throw if X does not have enough
% information for a meaningful answer, i.e. if we encounter an "instantiation error"
% ---

precondition_X_must_be_instantiated(X,Name,Ness,Tuned) :-
   assertion((Tuned==hard;Tuned==soft)),
   (
   nonvar(X)
   ->
   true
   ;
   (
      fail_if_not_hard(Tuned), % if this fails, the call fails (which is what we want)
      select_name(Name,Name2),
      format(string(Msg),"~s must not be uninstantiated. Can't check for '~s-ness'",[Name2,Ness]),
      throw_2(instantiation,Msg,X)
   )).

% special case precondition: the list

precondition_X_must_be_list(X,Name,Ness,Tuned) :-
   assertion((Tuned==hard;Tuned==soft)),
   (
   is_proper_list(X)
   ->
   true
   ;
   (
      fail_if_not_hard(Tuned), % if this fails, the call fails (which is what we want)
      select_name(Name,Name2),
      format(string(Msg),"~s must be a proper list. Can't check for '~s'-ness.",[Name2,Ness]),
      throw_2(type,Msg,X)
   )).

% special case precondition: the dict

precondition_X_must_be_dict(X,Name,Ness,Tuned) :-
   assertion((Tuned==hard;Tuned==soft)),
   (
   is_dict(X)
   ->
   true
   ;
   (
      fail_if_not_hard(Tuned), % if this fails, the call fails (which is what we want)
      select_name(Name,Name2),
      format(string(Msg),"~s must be a dict. Can't check for '~s'-ness.",[Name2,Ness]),
      throw_2(type,Msg,X)
   )).

% special case precondition: atomic (used to assess dict key)

precondition_X_must_be_atomic(X,Name,Ness,Tuned) :-
   assertion((Tuned==hard;Tuned==soft)),
   (
   atomic(X)
   ->
   true
   ;
   (
      fail_if_not_hard(Tuned), % if this fails, the call fails (which is what we want)
      select_name(Name,Name2),
      format(string(Msg),"~s must be atomic. Can't check for '~s'-ness.",[Name2,Ness]),
      throw_2(type,Msg,X)
   )).

% special case precondition: X must be instantiated enough to positively say whether it is cyclic

precondition_X_must_be_instantiated_enough_to_decide_whether_cyclic(X,Name,Tuned) :-
   assertion((Tuned==hard;Tuned==soft)),
   (
   var(X),
   !, % commit, then fail or throw
   fail_if_not_hard(Tuned), % if this fails, the call fails (which is what we want)
   select_name(Name,Name2),
   format(string(Msg),"~s must not be uninstantiated. Can't say anything about cyclic-ness.",[Name2]),
   throw_2(instantiation,Msg,X)).
precondition_X_must_be_instantiated_enough_to_decide_whether_cyclic(X,Name,Tuned) :-
   assertion((Tuned==hard;Tuned==soft)),
   (
   \+ground(X),
   acyclic_term(X),
   !, % commit, then fail or throw
   fail_if_not_hard(Tuned), % if this fails, the call fails (which is what we want)
   select_name(Name,Name2),
   format(string(Msg),"~s must not be 'nonground and cyclic'. Can't say anything about cyclic-ness.",[Name2]),
   throw_2(instantiation,Msg,X)).
precondition_X_must_be_instantiated_enough_to_decide_whether_cyclic(_,_,_). % default accepts anything

% ---
% Predicates which check whether the Tuned flag is set, and if so,
% construct an exception message and then throw via throw_2/3.
% Otherwise they fail.
%
% "Ness" is a a free string that expresses what was expected byut not seen
% For example, if "Ness" is "integer", the message will be about missing
% "integer-ness".
% ---

throw_or_fail_for_case_random(Tuned) :-
   fail_if_not_hard(Tuned), % if this fails, the call fails (which is what we want)
   throw_2(random,"random failure after calling maybe/1").

throw_or_fail(Error,X,Name,Tuned,Ness) :-
   fail_if_not_hard(Tuned), % if this fails, the call fails (which is what we want)
   select_name(Name,Name2),
   format(string(Msg),"~s should fulfill '~s-ness'",[Name2,Ness]),
   throw_2(Error,Msg,X).

throw_or_fail_with_message(Msg,X,Tuned) :-
   fail_if_not_hard(Tuned), % if this fails, the call fails (which is what we want)
   throw_2(type,Msg,X).

% ---
% Basement throwing predicate constructing the exception term itself.
% ---

throw_2(domain(Expected),Msg,Culprit)             :- throw(error(check(domain          ,Expected,Msg,Culprit),_)).
throw_2(type(Expected),Msg,Culprit)               :- throw(error(check(type            ,Expected,Msg,Culprit),_)).
throw_2(domain,Msg,Culprit)                       :- throw(error(check(domain          ,_       ,Msg,Culprit),_)).
throw_2(type,Msg,Culprit)                         :- throw(error(check(type            ,_       ,Msg,Culprit),_)).
throw_2(uninstantiation,Msg,Culprit)              :- throw(error(check(uninstantiation ,_       ,Msg,Culprit),_)). % ISO's "uninstantiation error"
throw_2(instantiation,Msg,Culprit)                :- throw(error(check(instantiation   ,_       ,Msg,Culprit),_)). % ISO's "instantiation error"

% These seem dubious:

throw_2(passall,Msg,Culprit)                      :- throw(error(check(passall         ,_       ,Msg,Culprit),_)).
throw_2(passany,Msg,Culprit)                      :- throw(error(check(passany         ,_       ,Msg,Culprit),_)).
throw_2(passnone,Msg,Culprit)                     :- throw(error(check(passnone        ,_       ,Msg,Culprit),_)).
throw_2(forall,Msg,Culprit)                       :- throw(error(check(forall          ,_       ,Msg,Culprit),_)).
throw_2(forany,Msg,Culprit)                       :- throw(error(check(forany          ,_       ,Msg,Culprit),_)).
throw_2(fornone,Msg,Culprit)                      :- throw(error(check(fornone         ,_       ,Msg,Culprit),_)).

% Having this at the end of throw_2/3 saves the day when debugging

throw_2(_,_,_)                                    :- throw("Bug! You forgot a throw_2/3 clause in the source").

throw_2(random,Msg)                               :- throw(error(check(random                   ,_       ,Msg,_      ),_)).

throw_various(Type,Msg,Culprit)                   :- throw(error(check(Type,_,Msg,Culprit),_)).

% ---
% Helpers for throwing
% ---

select_name(Name,"the value") :-
   (var(Name);Name=='';Name==""),
   !.
select_name(NameIn,NameOut) :-
   format(string(NameOut),"~q",[NameIn]).

% ---
% Properly printing the error(check(_,_,_,_),_) exception term
% by adding rules to the prolog::error_message//1 multifile DCG rule.
% ---

:- multifile prolog:error_message//1.  % 1-st argument of error term

prolog:error_message(check(Type,Expected,Msg,Culprit)) -->
    { build_main_text_pair(Type,MainTextPair) },
    [ MainTextPair, nl ],
    lineify_expected(Expected),
    lineify_msg(Msg),
    lineify_culprit(Culprit).

% ---
% Helpers for prolog:error_message(Formal)
% ---

extended_msg(domain,          "the culprit is outside the required domain").
extended_msg(type,            "the culprit is not of the required type").
extended_msg(uninstantiation, "the culprit is already (fully) instantiated").
extended_msg(instantiation,   "the culprit is not instantiated (enough)").
extended_msg(random,          "this is a random error due to the outcome of maybe/1").

make_sure_it_is_string(X,X) :-
   string(X),
   !.
make_sure_it_is_string(X,Str) :-
   atom(X),
   !,
   atom_string(X,Str).
make_sure_it_is_string(X,Str) :-
   format(string(Str),"~q",[X]).

build_main_text_pair(Type,MainTextPair) :-
   extended_msg(Type,ExMsg),
   !,
   make_sure_it_is_string(ExMsg,ExMsgStr),
   MainTextPair = 'check failed : ~q error (~s)'-[Type,ExMsgStr].
build_main_text_pair(Type,MainTextPair) :-
   MainTextPair = 'check failed : ~q error'-[Type].

lineify_expected(Expected) -->
   { nonvar(Expected), make_sure_it_is_string(Expected,ExpectedStr) },
   !,
   [ '   expected  : ~s'-[ExpectedStr], nl ].
lineify_expected(_) --> [].

lineify_msg(Msg) -->
   { nonvar(Msg), make_sure_it_is_string(Msg,MsgStr) },
   !,
   [ '   message   : ~s'-[MsgStr], nl ].
lineify_msg(_) --> [].

lineify_culprit(Culprit) -->
   { nonvar(Culprit), make_sure_it_is_string(Culprit,CulpritStr) },
   !,
   [ '   culprit   : ~s'-[CulpritStr], nl ].
lineify_culprit(_) --> [].

% ----
% The "type test" for inty terms: accepts an integer
% or a float that represents an integer. Anything else
% causes failure to type error.
% ---

just_an_inty(X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"inty",TP),
   ((integer(X);inty_float(X))
    ->
    true
    ;
    throw_or_fail(type(int_or_float),X,Name,Tuned,"inty")).

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
% eval(Check,TermToCheck,NameOfTerm,Tuned,PreconditionTuned)
% This predicate needs no internal cuts as a unique decision is taken on arg 1
% ---

eval(true,_,_,_,_).
eval(false,_,_,_,_).
eval(fail,_,_,_,_).

eval(var,X,Name,Tuned,_TP) :-
   var(X)
   ->
   true
   ;
   throw_or_fail(uninstantiation,X,Name,Tuned,"var").

eval(nonvar,X,Name,Tuned,_TP) :-
   nonvar(X)
   ->
   true
   ;
   throw_or_fail(instantiation,X,Name,Tuned,"nonvar").

eval(ground,X,Name,Tuned,_TP) :-
   ground(X)
   ->
   true
   ;
   throw_or_fail(domain,X,Name,Tuned,"ground").

eval(nonground,X,Name,Tuned,_TP) :-
   \+ground(X)
   ->
   true
   ;
   throw_or_fail(domain,X,Name,Tuned,"nonground").

eval(atom,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"atom",TP),
   (atom(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"atom")).

eval(symbol,X,Name,Tuned,TP) :-
   eval(atom,X,Name,Tuned,TP).

eval(atomic,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"atomic",TP),
   (atomic(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"atomic")).

eval(constant,X,Name,Tuned,TP) :-
   eval(atomic,X,Name,Tuned,TP).

eval(compound,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"compound",TP),
   (compound(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"compound")).

eval(boolean,X,Name,Tuned,TP) :-
   eval(atom,X,Name,Tuned,TP),
   ((X==true;X==false)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"boolean")).

eval(pair,X,Name,Tuned,TP) :-
   eval(compound,X,Name,Tuned,TP),
   (X = -(_,_)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"pair")).

eval(string,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"string",TP),
   (string(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"string")).

eval(stringy,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"stringy",TP),
   ((atom(X);string(X))
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"stringy")).

eval(nonempty_stringy,X,Name,Tuned,TP) :-
   eval(stringy,X,Name,Tuned,TP),
   ((X\=='',X\== "")
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"nonempty stringy")).

eval(char,X,Name,Tuned,TP) :-
   eval(atom,X,Name,Tuned,TP),
   % Note that we need to test atom/1 first because atom_length/2 transforms-to-atom!
   % atom_length/2 may be too wasteful to test for a precise length (?)
   (atom_length(X,1)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"char")).

eval(code,X,Name,Tuned,TP) :-
   eval(integer,X,Name,Tuned,TP),
   ((0=<X,X=<0x10FFFF)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"code")).

eval(chary,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"chary",TP),
   (integer(X)
    ->
       ((0=<X,X=<0x10FFFF)
        ->
        true
        ;
        throw_or_fail(domain,X,Name,Tuned,"code"))
    ;
    atom(X)
    ->
       (atom_length(X,1)
        ->
        true
        ;
        throw_or_fail(domain,X,Name,Tuned,"char"))
    ;
    throw_or_fail(type,X,Name,Tuned,"chary")).

eval(number,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"number",TP),
   (number(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"number")).

eval(float,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"float",TP),
   (float(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"float")).

eval(int,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"integer",TP),
   (integer(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"integer")).

eval(integer,X,Name,Tuned,TP) :-
   eval(int,X,Name,Tuned,TP).

eval(rational,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"rational",TP),
   (rational(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"rational")).

eval(nonint_rational,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"nonint_rational",TP),
   (rational(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"nonint_rational")),
   (integer(X)
    ->
    throw_or_fail(domain,X,Name,Tuned,"nonint_rational")
    ;
    true).

eval(proper_rational,X,Name,Tuned,TP) :-
   eval(nonint_rational,X,Name,Tuned,TP).

eval(negnum,X,Name,Tuned,TP) :-
   eval(number,X,Name,Tuned,TP),
   ((X < 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"strictly negative number")).

eval(negnumber,X,Name,Tuned,TP) :-
   eval(negnum,X,Name,Tuned,TP).

eval(posnum,X,Name,Tuned,TP) :-
   eval(number,X,Name,Tuned,TP),
   ((X > 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"strictly positive number")).

eval(posnumber,X,Name,Tuned,TP) :-
   eval(posnum,X,Name,Tuned,TP).

eval(neg0num,X,Name,Tuned,TP) :-
   eval(number,X,Name,Tuned,TP),
   ((X =< 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"number that is =< 0")).

eval(neg0number,X,Name,Tuned,TP) :-
   eval(neg0num,X,Name,Tuned,TP).

eval(pos0num,X,Name,Tuned,TP) :-
   eval(number,X,Name,Tuned,TP),
   ((X >= 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"number that is >= 0")).

eval(pos0number,X,Name,Tuned,TP) :-
   eval(pos0num,X,Name,Tuned,TP).

eval(non0num,X,Name,Tuned,TP) :-
   eval(number,X,Name,Tuned,TP),
   ((X =\= 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"number that is not 0")).

eval(non0number,X,Name,Tuned,TP) :-
   eval(non0num,X,Name,Tuned,TP).

eval(float_not_nan,X,Name,Tuned,TP) :-
   eval(float,X,Name,Tuned,TP),
   ((NaN is nan,X \== NaN) % arithmetic comparison would fail
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"float that is not NaN")).

eval(float_not_inf,X,Name,Tuned,TP) :-
   eval(float,X,Name,Tuned,TP),
   ((X =\= -1.0Inf,X =\= +1.0Inf)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"float that is not positive or negative infinity")).

eval(float_not_neginf,X,Name,Tuned,TP) :-
   eval(float,X,Name,Tuned,TP),
   ((X =\= -1.0Inf)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"float that is not negative infinity")).

eval(float_not_posinf,X,Name,Tuned,TP) :-
   eval(float,X,Name,Tuned,TP),
   ((X =\= +1.0Inf)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"float that is not positive infinity")).

eval(negint,X,Name,Tuned,TP) :-
   eval(int,X,Name,Tuned,TP),
   ((X<0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"strictly negative integer")).

eval(negative_integer,X,Name,Tuned,TP) :-
   eval(negint,X,Name,Tuned,TP).

eval(posint,X,Name,Tuned,TP) :-
   eval(int,X,Name,Tuned,TP),
   ((X>0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"strictly positive integer")).

eval(positive_integer,X,Name,Tuned,TP) :-
   eval(posint,X,Name,Tuned,TP).

eval(neg0int,X,Name,Tuned,TP) :-
   eval(int,X,Name,Tuned,TP),
   ((X =< 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"integer that is =< 0")).

eval(pos0int,X,Name,Tuned,TP) :-
   eval(int,X,Name,Tuned,TP),
   ((X >= 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"integer that is >= 0")).

eval(nonneg,X,Name,Tuned,TP) :-
   eval(pos0int,X,Name,Tuned,TP).

eval(inty,X,Name,Tuned,TP) :-
   just_an_inty(X,Name,Tuned,TP).

eval(neginty,X,Name,Tuned,TP) :-
   eval(inty,X,Name,Tuned,TP),
   (((integer(X),X<0);(float(X),X<0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"strictly negative inty")).

eval(posinty,X,Name,Tuned,TP) :-
   eval(inty,X,Name,Tuned,TP),
   (((integer(X),X>0);(float(X),X>0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"strictly positive inty")).

eval(neg0inty,X,Name,Tuned,TP) :-
   eval(inty,X,Name,Tuned,TP),
   (((integer(X),X=<0);(float(X),X=<0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"inty that is =< 0")).

eval(pos0inty,X,Name,Tuned,TP) :-
   eval(inty,X,Name,Tuned,TP),
   (((integer(X),X>=0);(float(X),X>=0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"inty that is >= 0")).

eval(negfloat,X,Name,Tuned,TP) :-
   eval(float_not_nan,X,Name,Tuned,TP),
   (X<0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"strictly negative float")).

eval(posfloat,X,Name,Tuned,TP) :-
   eval(float_not_nan,X,Name,Tuned,TP),
   (X>0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"strictly positive float")).

eval(neg0float,X,Name,Tuned,TP) :-
   eval(float_not_nan,X,Name,Tuned,TP),
   (X=<0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"float that is =< 0")).

eval(pos0float,X,Name,Tuned,TP) :-
   eval(float_not_nan,X,Name,Tuned,TP),
   (X>=0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"float that is >= 0")).

eval(list,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"list",TP),
   (is_proper_list(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"proper list")).

eval(proper_list,X,Name,Tuned,TP) :-
   eval(list,X,Name,Tuned,TP).

eval(nonempty_list,X,Name,Tuned,TP) :-
   eval(list,X,Name,Tuned,TP),
   (X \== []
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"proper nonempty list")).

eval(dict,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"dict",TP),
   (is_dict(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Tuned,"dict")).

eval(stringy_typeid,X,Name,Tuned,TP) :-
   eval(atom,X,Name,Tuned,TP),
   ((X==atom;X==string)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"stringy_typeid")).

eval(chary_typeid,X,Name,Tuned,TP) :-
   eval(atom,X,Name,Tuned,TP),
   ((X==char;X==code)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"chary_typeid")).

eval(char_list,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"char_list",TP),
   precondition_X_must_be_list(X,Name,"char_list",Tuned), % Dual traversal, but one is in C, so this may be faster than "unifying" to a single traversal.
   forall(member(MX,X),eval(char,MX,Name,Tuned,TP)). % TODO: Open up to get at the index

eval(chars,X,Name,Tuned,TP) :-
   eval(char_list,X,Name,Tuned,TP).

eval(code_list,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"code_list",TP),
   precondition_X_must_be_list(X,Name,"code_list",Tuned), % Dual traversal, but one is in C, so this may be faster than "unifying" to a single traversal.
   forall(member(MX,X),eval(code,MX,Name,Tuned,TP)). % TODO: Open up to get at the index

eval(codes,X,Name,Tuned,TP) :-
   eval(code_list,X,Name,Tuned,TP).

eval(chary_list,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"chary_list",TP),
   precondition_X_must_be_list(X,Name,"chary_list",Tuned), % Dual traversal, but one is in C, so this may be faster than "unifying" to a single traversal.
   eval(forany([chars,codes]),X,Name,Tuned,TP).  % TODO: Open up to get at the index and get better error messages
   % Simple, but the error is confusing for "check_that([a,2],hard(chary_list))" for example and does not give the index

eval(charys,X,Name,Tuned,TP) :-
   eval(chary_list,X,Name,Tuned,TP).

eval(member(ListOfValues),X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"list_membership",TP),
   precondition_X_must_be_list(ListOfValues,Name,"list_membership",TP), % predicate name is confusing here: it is not X but ListOfValues which must be a list!
   ((\+ \+ member(X,ListOfValues)) % Use \+ \+ to roll back any accidental bindings
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"list_membership")).

eval(dict_has_key(Key),X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"dict-has-key",TP),
   precondition_X_must_be_dict(X,Name,"dict-has-key",TP),
   precondition_X_must_be_instantiated(Key,Name,"dict-has-key",TP),
   precondition_X_must_be_atomic(Key,Name,"dict-has-key",TP),
   (get_dict(Key,X,_)
    ->
    true
    ;
    throw_or_fail(domain,[dict-X,key-Key],Name,Tuned,"dict-has-key")). % domain error sounds right here for "key not in dict"

eval(random(Probability),_,_,Tuned,_) :-
   maybe(Probability)  % throws type error on value not in [0.0,1.0]
   ->
   true
   ;
   throw_or_fail_for_case_random(Tuned).

eval(fail(Msg),X,_,Tuned,_) :-
   throw_or_fail_with_message(Msg,X,Tuned).

eval(unifies(Z),X,_,_,_) :-
   \+ \+ (Z = X).

eval(acyclic_now,X,Name,Tuned,_) :-
   acyclic_term(X) % never throws
   ->
   true
   ;
   throw_or_fail(domain,X,Name,Tuned,"acyclic_now"). % is domain right here?

eval(cyclic_now,X,Name,Tuned,_) :-
   cyclic_term(X) % never throws
   ->
   true
   ;
   throw_or_fail(domain,X,Name,Tuned,"cyclic_now"). % is domain right here?

eval(acyclic_forever,X,Name,Tuned,_) :-
   (ground(X),acyclic_term(X)) % never throws
   ->
   true
   ;
   throw_or_fail(domain,X,Name,Tuned,"acyclic_forever"). % is domain right here?

eval(cyclic,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated_enough_to_decide_whether_cyclic(X,Name,TP),
   (cyclic_term(X)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Tuned,"cyclic")). % is domain right here?

eval(stream,X,Name,Tuned,TP) :-
   precondition_X_must_be_instantiated(X,Name,"stream",TP),
   (atom(X)
    ->
       (is_stream(X)
        ->
        true
        ;
        throw_or_fail(domain,X,Name,Tuned,"atom-naming-a-stream"))
    ;
    atomic(X)
    ->
       (is_stream(X)
        ->
        true
        ;
        throw_or_fail(domain,X,Name,Tuned,"atomic-designating-a-stream"))
    ;
    throw_or_fail(type,X,Name,Tuned,"atom-or-atomic")).

% ---
% eval with forall/forany/fornone
% ---

eval(forall(ListOfChecks),X,Name,Tuned,TP) :-
   forall_forall_loop(ListOfChecks,X,Name,Tuned,TP) % Tuned is passed
   ->
   true
   ;
   throw_or_fail(forall,checks(ListOfChecks)-item(X),Name,Tuned,"all of the checks succeed for the item").

eval(forany(ListOfChecks),X,Name,Tuned,TP) :-
   forany_forall_loop(ListOfChecks,X,Name,TP) % Tuned is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(forany,checks(ListOfChecks)-item(X),Name,Tuned,"at least one of the checks succeeds for the item").

eval(fornone(ListOfChecks),X,Name,Tuned,TP) :-
   fornone_forall_loop(ListOfChecks,X,Name,TP) % Tuned is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(fornone,checks(ListOfChecks)-item(X),Name,Tuned,"none of the checks succeeds for the item").

% ---
% eval with passall/passany/passnone
% TODO: - We give not precise indicates which X failed the check how (i..e no index)
%       - Also, passany and passnone do not give any information about what went wrong at
%         all as they fail the individual checks and then throw a generic exception
%       Is fixing both of these worth the complexity?
% ---

eval(passall(Check),ListOfX,Name,Tuned,TP) :-
   passall_forall_loop(Check,ListOfX,Name,Tuned,TP) % Tuned is passed; thrown exception informs about problem
   ->
   true
   ;
   throw_or_fail(passall,check(Check)-items(ListOfX),Name,Tuned,"all of the items pass the check").

eval(passany(Check),ListOfX,Name,Tuned,TP) :-
   passany_forall_loop(Check,ListOfX,Name,TP) % Tuned is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(passany,check(Check)-items(ListOfX),Name,Tuned,"at least one of the items passes the check").

eval(passnone(Check),ListOfX,Name,Tuned,TP) :-
   passnone_forall_loop(Check,ListOfX,Name,TP) % Tuned is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(passnone,check(Check)-items(ListOfX),Name,Tuned,"none of the items passes the check").

% ---
% More helpers
% ---

forall_forall_loop(ListOfChecks,X,Name,Tuned,TP) :-
   % TODO: There is no need to do this if the well-formedness check
   % is on, but it must be done either there or here otherwise the forall
   % call will succeed on non-list (similarly, in forany/fornone)
   eval(proper_list,ListOfChecks,Name,hard,hard),
   forall(                           % success of ListOfChecks is empty
      member(Check,ListOfChecks),
      eval(Check,X,Name,Tuned,TP)).

forany_forall_loop(ListOfChecks,X,Name,TP) :-
   eval(proper_list,ListOfChecks,Name,hard,hard),
   \+forall(                         % failure if ListOfChecks is empty
     member(Check,ListOfChecks),
     \+eval(Check,X,Name,soft,TP)).  % disable throwing

fornone_forall_loop(ListOfChecks,X,Name,TP) :-
   eval(proper_list,ListOfChecks,Name,hard,hard),
   (ListOfChecks == [])              % force failure if ListOfChecks is empty
   ->
   false
   ;
   forall(
      member(Check,ListOfChecks),
      \+eval(Check,X,Name,soft,TP)). % disable throwing

passall_forall_loop(Check,ListOfX,Name,Tuned,TP) :-
   % TODO: There is no need to do this if the well-formedness check
   % is on, but it must be done either there or here otherwise the forall
   % call will succeed on non-list
   eval(proper_list,ListOfX,Name,hard,hard),
   forall(                           % success if ListOfX is empty
      member(X,ListOfX),
      eval(Check,X,Name,Tuned,TP)).

passany_forall_loop(Check,ListOfX,Name,TP) :-
   eval(proper_list,ListOfX,Name,hard,hard),
   \+forall(                         % failure if ListOfX is empty
      member(X,ListOfX),
      \+eval(Check,X,Name,soft,TP)). % disable throwing

passnone_forall_loop(Check,ListOfX,Name,TP) :-
   eval(proper_list,ListOfX,Name,hard,hard),
   (ListOfX == [])                   % force failure if ListOfX is empty
   ->
   false
   ;
   forall(
      member(X,ListOfX),
      \+eval(Check,X,Name,soft,TP)). % disable throwing

