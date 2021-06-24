:- module(onepointfour_basics_checks_wellformed,
          [
            wellformed_conds_or_throw/2 
           ,atomoform_checks/1
           ,exists_cond_or_throw/1
          ]).

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library('onepointfour_basics/checks/throwers.pl')).

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

% called from the main predicates to verify a list of conditions.

wellformed_conds_or_throw(Conditions,X) :-
   wellformed_conds(Conditions,X) % may also throw
   ->
   true
   ;
   throw_2(syntax,"conditions do not pass syntax check",Conditions).

% wellformed_conds(Conditions,X)
%
% Verify the conds of Conditions for syntactic correctness. This is done
% outside of actual check-evaluation eval/4, to have clean code and to be able to
% disable the verification for well-formedness by (currently) commenting out
% the call to wellformed/2.

wellformed_conds([Condition|More],X) :-
   exists_cond_or_throw(Condition),
   wellformed_cond_or_throw(Condition,X),
   wellformed_conds(More,X).
wellformed_conds([],_).

% ---

exists_cond_or_throw(Condition) :-
   exists_cond(Condition)
   ->
   true
   ;
   throw_2(unknown_condition,"unknown condition found during syntax check",Condition).

% Verify the form of a single condition.

exists_cond(break(_Check)).
exists_cond(smooth(_Check)).
exists_cond(soft(_Check)).
exists_cond(tuned(_Check)).
exists_cond(hard(_Check)).

% ---

wellformed_cond_or_throw(Condition,X) :-
   atom(Condition)
   ->
   true
   ;
   (Condition =.. [_,Check], wellformed_check_or_throw(Check,X)).

% ---

wellformed_check_or_throw(Check,X) :-
   wellformed_check_2(Check,X)
   ->
   true
   ;
   throw_2(unknown_or_problematic_check,"unknown or problematic check found during syntax check",Check).

% ---
% Check whether a single 'check' (which has been removed from its 'condition') is well-formed
% ---

% If the 'check' is an atom, we just need to see whether it's one of the allowed atoms

wellformed_check_2(Check,_) :- 
   atom(Check),
   !,
   atomoform_checks(AFCs),
   memberchk(Check,AFCs).

% If it's a compound term with functor 'member', then we allow either a single (possibly 
% empty) list as argument to member/1 or >= 1 arguments that are packed into a list.
% The latter makes it unnecessary to add brackets, which increases legibility. But 
% we can't check anything about the arguments of member.

wellformed_check_2(member(_)            ,_).  % argument  can be anything, maybe even a list
wellformed_check_2(member(_,_)          ,_).  % arguments can be anything
wellformed_check_2(member(_,_,_)        ,_).  % arguments can be anything
wellformed_check_2(member(_,_,_,_)      ,_).  % arguments can be anything
wellformed_check_2(member(_,_,_,_,_)    ,_).  % arguments can be anything
wellformed_check_2(member(_,_,_,_,_,_)  ,_).  % arguments can be anything
wellformed_check_2(member(_,_,_,_,_,_,_),_).  % arguments can be anything

% Various other compound term.
% Sometimes we just verify the form. For example, for dict_has_key/2 we leave the testing
% of Key and Dict type to the actual check call.

wellformed_check_2(dict_has_key(_),_).
wellformed_check_2(type(ListOfTypes),_) :- 
   is_proper_list(ListOfTypes),
   atomoform_checks(AFCs),
   maplist({AFCs}/[T]>>memberchk(T,AFCs),ListOfTypes).
wellformed_check_2(random(Probability),_) :-
   number(Probability),
   0=<Probability,
   Probability=<1.
wellformed_check_2(unifies(_),_).
wellformed_check_2(forall(ListOfChecks),X) :-
   wellformed_list_of_checks(ListOfChecks,X).
wellformed_check_2(forany(ListOfChecks),X) :-
   wellformed_list_of_checks(ListOfChecks,X).
wellformed_check_2(fornone(ListOfChecks),X) :-
   wellformed_list_of_checks(ListOfChecks,X).
wellformed_check_2(passall(Check),ListOfX) :-
   wellformed_check_over_list(Check,ListOfX).
wellformed_check_2(passany(Check),ListOfX) :-
   wellformed_check_over_list(Check,ListOfX).
wellformed_check_2(passnone(Check),ListOfX) :-
   wellformed_check_over_list(Check,ListOfX).

% Specialized verification

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

% The list of atoms for "elementary checks", i.e. those that are not a compound term, are listed here.

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

% Helpers

is_proper_list_or_throw(Check,ListOfX) :-
   is_proper_list(ListOfX)
   ->
   true
   ;
   throw_2(type,"check needs a list as argument",[check(Check),arg(ListOfX)]).


is_proper_list(L) :- is_list(L).
