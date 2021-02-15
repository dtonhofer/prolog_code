:- module(onepointfour_basics_throwme,
          [
          throwme/3 % throwme(+LookupPred,+LookupTerm,+Module)
          ]).

:- use_module(library(solution_sequences)).

/** <module> Helps move exception messages out of code

To make use of this, declare "exception descriptors" 
in the form of clauses of predicate =|exc_desc/5|= in your module.
The predicate =|exc_desc/5|= should not be exported from the module.

When an exception is to be thrown, your code should call throwme/3
with an appropriate _LookupPred_ and _LookupTerm_ and the name of the
module in which the =|exc_desc/5|= can be found. throwme/3 can access
=|exc_desc/5|= by qualifying it with the module name, even though it is
not exported (SWI-Prolog does not enforce encapsulation).
     
=|exc_desc/5|= clauses contain the code composing exception message
which can be maintained outside of principal code. 

Example:

```
:- module(foo,[main/1]). 
:- use_module(library('onepointfour_basics/throwme.pl')).
   
main(X) :-
   (integer(X) 
   -> true 
    ;  throwme(main,not_integer(X),foo)).

exc_desc(main,not_integer(X),Location,Formal,Msg) :-
   Location = main/0,
   Formal   = type_error(integer,X),
   Msg      = "main/1 called with non-integer".
```   

See the file =|throwme_example.pl|= for a more extensive example.
      
## Exception descriptors

Exception descriptors are defined in the code which imports
the present module. They are
clauses (possibly facts) of the predicate =|exc_desc/5|=:

=|exc_desc(+LookupPred,+LookupTerm,?Location,?Formal,?Msg)|=

The first two arguments are used for lookup. The last three arguments 
are "output values" which are used to construct the exception term that
will be thrown.

If Location is left an unbound variable, it can be instantiated to a 
stack trace (also known as a back trace) if the exception reaches the
Prolog Toplevel or is caught by catch_with_backtrace/3. Otherwise, 
"Location" should be a predicate indicator or something similar.

## History

   1. 2020-07-30: First version as a spinoff of the review of JPL bridge.
   1. 2020-08-05: Code reviewed.
   1. 2021-01-20: Changed comments.
   1. 2021-02-09: Adapted comments to pldoc. Reorganized for an SWI-Prolog "pack". 
   1. 2021-02-15: Better documentation.
   
## More

@license [Zero-Clause BSD / Free Public License 1.0.0 (0BSD)](https://opensource.org/licenses/0BSD)
@author David Tonhofer (ronerycoder@gluino.name)
@tbd A tool that extracts throwme/3 calls from Prolog code and that puts 
  them into a 
  [plunit](https://eu.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/plunit.html%27%29)
  block to exercise them. It is unlikely that tests will excercise all 
  the throwme/3 calls, so and extract-and-separately-unit-test approach
  to make sure they have an =|exc_desc/5|= in the codebase seems a good idea.
  One would still have to do some manual work to add the use_module/2 calls 
  and maybe rewrite the arguments. 
@bug The toplevel reports the exception as having been throw from the 
  onepointfour_basics_throwme:throwme_help/3, which is quite correct. On needs
  to shave off 2 stack frames.
@bug As an alternative to passing the module name, on could declare throwme/3 
  as a meta-predicate that is passed the argument-less version of throwme/3,
  i.e. just the atom, as 3rd argument. Would that work?
  
*/


%! throwme(+LookupPred,+LookupTerm,+Module)
%
% Predicate called to construct an exception term and throw it. It collects
% applicable exception descriptors by calling 
%
% ```
% findall(
%    [Location,Formal,Msg],
%    limit(2,Module:exc_desc(LookupPred,LookupTerm,Location,Formal,Msg)),
%    ExcDescList)
% ```
%
% which collects the three last arguments of at most two successful calls to
% predicate =|Module:exc_desc/5|=.
%
% _Location_, _Formal_, _Msg_ are supposed to be instantiated by
% exactly once. If there are no solutions or multiple solutions, an
% error message about that problem is generated instead.
%
% The module _Module_ does not need to export its =|exc_desc/5|= 
% predicate: SWI-Prolog does not enforce encapsulation.
%
% The arguments have this meaning:
%
%    - LookupPred : What predicate is throwing; this is an atom (a keyword) 
%      generally shaped after the actual predicate name of the throwing 
%      predicate. It is _not_ a predicate indicator.
%    - LookupTerm : A term, possibly compound, that describes the problem
%      somehow. It is both programmer-interpretable (but still abstract) as 
%      well as a way of passing values that can be inserted into the _Formal_
%      of an exception part.

% limit to at most two to avoid potential trouble

throwme(LookupPred,LookupTerm,Module) :-
   findall([Location,Formal,Msg],
           limit(2,Module:exc_desc(LookupPred,LookupTerm,Location,Formal,Msg)),
           ExcDescList),
   length(ExcDescList,ExcDescCount),
   throwme_help(ExcDescCount,ExcDescList,LookupPred,LookupTerm).

%! throwme_help(+ExcDescCount,+ExcDescList,LookupPred,LookupTerm)
% 
% Build an exception term and throw it. The ExcDescList contains a list
% of ExcDescsCount matching exception descriptions =|[Location,Formal,Msg]|=
% as collected by calling the goal =|exc_desc(LookupPred,LookupTerm,Location,Formal,Msg)|=.
%
% The constructed and thrown exception term is "quasi ISO-standard" having the
% structure =|error(Formal,Context)|=. 
% 
%    - There is not guarantee that the _Formal_ term is any of the 
%      allowed _Formal_ terms listed in the ISO-standard. In fact, this is generally
%      not the case, hence "quasi ISO-standard".
%    - The _Context_ term (about which the ISO standard says nothing,
%      leaving it "implementation-defined") is structured according to SWI-Prolog 
%      conventions: it is a term =|context(Location,Msg)|= where _Location_ 
%      is often filled with the predicate indicator of the throwing predicate.
%      If _Location_ is left an unbound variable, that variable can be instantiated
%      to a stack trace (either on the toplevel or by a catching the exception with
%      catch_with_backtrace/3.) _Msg_ should be a "stringy thing" to print out, 
%      i.e. a human-readable explainer that is either an atom or an SQI-Prolog string,
%      possibly constructed dynamically in an exc_desc/5.
%
% Two cases: 
%
% ** Exactly 1 applicable "exception descriptor" could be found**
%
% This is the usual case and we just need to throw the corresponding exception.
% This is handled by the first clause.
%
% ** 0 or more than 1 applicable "exception descriptor" could be found** 
%
% That means the set of exception descriptors is incomplete/ambiguous or the lookup
% goal is wrongly formulated. Here we throws a "quasi ISO-standard" exception 
% with a term =|error(Formal,Context)|= but with the formal term the non-ISO 
% atom 'programming_error'. 
%
%    - Note that _Msg_ is an atom, not a string (is that ok? It should probably
%      be a String, at least in SWI-Prolog)
%    - Note that the second argument of =|error(_,_)|= follows SWI-Prolog 
%      conventions, i.e. we throw =|error(_,context(Location,Msg))|=
%      and if _Location_ is an unbound variable, it can be instantiated witj
%      a backtrace.

throwme_help(1,[[Location,Formal,Msg]],_,_) :-
   throw(error(Formal,context(Location,Msg))).

throwme_help(Count,_,LookupPred,LookupTerm) :-
   Count \== 1,
   with_output_to(
      atom(Msg),
      format("Instead of 1, found ~d exception descriptors for LookupPred = ~q, LookupTerm = ~q", 
         [Count,LookupPred,LookupTerm])),
   throw(error(programming_error,context(_,Msg))).

   
