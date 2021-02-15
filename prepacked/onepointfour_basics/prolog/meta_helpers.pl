:- module(onepointfour_basics_meta_helpers,
          [
          switch/4                 % switch(If1,Then1,If2,Then2)  (throws if 'else' condition is hit)
         ,switch/5                 % switch(If1,Then1,If2,Then2,Else)
         ,switch/6                 % switch(If1,Then1,If2,Then2,If3,Then3)  (throws if 'else' condition is hit)
         ,switch/7                 % switch(If1,Then1,If2,Then2,If3,Then3,Else)
         ,switch/8                 % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4)  (throws if 'else' condition is hit)
         ,switch/9                 % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,Else)
         ,switch/10                % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5)  (throws if 'else' condition is hit)
         ,switch/11                % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,Else)
         ,switch/12                % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,If6,Then6)  (throws if 'else' condition is hit)
         ,switch/13                % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,If6,Then6,Else)
         ,if_then_else/3           % if_then_else(Condition,Then,Else)
         ,reify_outcome/4          % reify_outcome(Condition,SuccessThing,FailureThing,Out) (unifies "Out" with either "SuccessThing" or "FailureThing")
         ,reify/2                  % reify(Goal,Outcome) (unifies "Outcome" with either 'true' or 'false') (should properly be 'true' or 'fail')
         ,if_then/2                % if_then(Condition,Then) (nothing happens if the 'else' condition is hit as "Condition" fails)
         ,unless/2                 % unless(Condition,Else)  (nothing happens if the 'then' condition is hit as "Condition" succeeds)
         ,maplist_onto_open_list/4 % maplist_onto_open_list(Goal,ListIn,TipOfListOut,FinOfListOut) -- TipOfListOut-FinOfListOut is a difference list of an open list
         ]).

/** <module> Various metapredicates

This module provides meta predicates try to make Prolog code easier to
read and write. One can advantageously use those instead of possibly
imbricated =|->/2|= and =|;/2|=, which become unreadable fast.

   * =|switch/N|=
      * Replaces a sequence of =|-> ;|= constructs.
      * May have a default case or not.
      * If not, and the default case is hit anyway, an exception is thrown.
   * =|if_then_else/3|=
      * Replaces a single =|-> ;|=.
   * =|if_then/2|=
      * Replaces a single =|-> ; true|=.
   * =|unless/2|=
      * As in Perl. Replaces a single =|-> true ;|=.

And also statements to reify success/failure:

   * =|reify/2|=
   * =|reify_outcome/4|=

For appending transformed elements onto an open list:

   * =|maplist_onto_open_list/4|=

These are all metapredicates though, and thus may cause significant
slowdown. A better solution would be to rewrite the syntactic
constructs into Prolog-standard `->` before compiling.

Load module with:

```
?- use_module(library('onepointfour_basics/meta_helpers.pl')).
```

## Examples:

```
:- use_module(library('onepointfour_basics/meta_helpers.pl')).

val_string(X,S) :-
   switch(
      between(0,10,X),
      S="between 0 and 10",
      between(11,20,X),
      S="between 11 and 20",
      between(21,30,X),
      S="between 21 and 30",
      between(31,40,X),
      S="between 31 and 40",
      S="above 40").

?- val_string(4,S).
S = "between 0 and 10".

?- reify_outcome((random(X),X>0.5),["That went well",X],["That didn't work",X],Out).
X = 0.7658474306692046,
Out = ["That went well",0.7658474306692046].

?- reify_outcome((random(X),X>0.5),["That went well",X],["That didn't work",X],Out).
Out = ["That didn't work",X].

?- maplist_onto_open_list([X,O]>>atomic_list_concat(['<',X,'>'],O),[foo,bar,baz],Tip,Fin), 
   maplist_onto_open_list([X,O]>>atomic_list_concat(['#',X,'#'],O),[foo,bar,baz],Fin,Fin2).
Tip = ['<foo>','<bar>','<baz>','#foo#','#bar#','#baz#'|Fin2],
Fin = ['#foo#','#bar#','#baz#'|Fin2].
```

## History

   1. 2021-01-19: Review.
   1. 2021-02-02: Added maplist_onto_open_list/4 while writing dict prettyprinting code. Reviewed comments.
   1. 2021-02-09: Adapted comments to pldoc. Reorganized for an SWI-Prolog "pack".

## More

   @license [Zero-Clause BSD / Free Public License 1.0.0 (0BSD)](https://opensource.org/licenses/0BSD)
   @author David Tonhofer (ronerycoder@gluino.name)
*/


% ---
% See
% https://eu.swi-prolog.org/pldoc/doc_for?object=(meta_predicate)/1
% for an explanation of the block below
% ---

:- meta_predicate
       switch(0,0,0,0)
      ,switch(0,0,0,0,0)
      ,switch(0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0,0,0,0,0,0)
      ,if_then_else(0,0,0)
      ,reify_outcome(0,?,?,?)
      ,reify(0,?)
      ,if_then(0,0)
      ,unless(0,0)
      ,maplist_onto_open_list(2,?,?,?). % Goal takes 2 args more than present in the term

%! switch(:If1,:Then1,:If2,:Then2)
%
% =switch/4=: pass 4 goals. Throws if the _else_ case =|\+(If1), \+(If2)|= is hit.

switch(If1,Then1,If2,Then2) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   unhandled_else_error.

%! switch(:If1,:Then1,:If2,:Then2,:Else)
%
% =switch/5=: pass 5 goals. Has _else_ case.

switch(If1,Then1,If2,Then2,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(Else).

%! switch(:If1,:Then1,:If2,:Then2,:If3,:Then3)
%
% =switch/6=: pass 6 goals. Throws if the _else_ case =|\+(If1), \+(If2), \+(If3)|= is hit.

switch(If1,Then1,If2,Then2,If3,Then3) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   unhandled_else_error.

%! switch(:If1,:Then1,:If2,:Then2,:If3,:Then3,:Else)
%
% =switch/7=: pass 7 goals. Has _else_ case.

switch(If1,Then1,If2,Then2,If3,Then3,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(Else).

%! switch(:If1,:Then1,:If2,:Then2,:If3,:Then3,:If4,:Then4)
%
% =switch/8=: pass 8 goals. Throws if the _else_ case =|+(If1), \+(If2), \+(If3), \+(If4)|= is hit.

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   unhandled_else_error.

%! switch(:If1,:Then1,:If2,:Then2,:If3,:Then3,:If4,:Then4,:Else)
%
% =switch/9=: pass 9 goals. Has _else_ case.

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   call(Else).

%! switch(:If1,:Then1,:If2,:Then2,:If3,:Then3,:If4,:Then4,:If5,:Then5)
%
% =switch/10=: pass 10 goals. Throws if the _else_ case =|+(If1), \+(If2), \+(If3), \+(If4), \+(If5)|= is hit.

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   call(If5)
   ->  call(Then5)
   ;   unhandled_else_error.

%! switch(:If1,:Then1,:If2,:Then2,:If3,:Then3,:If4,:Then4,:If5,:Then5,:Else)
%
% =switch/11=: pass 11 goals. Has _else_ case.

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   call(If5)
   ->  call(Then5)
   ;   call(Else).

%! switch(:If1,:Then1,:If2,:Then2,:If3,:Then3,:If4,:Then4,:If5,:Then5,:If6,:Then6)
%
% =switch/12=: pass 12 goals. Throws if the _else_ case =|+(If1), \+(If2), \+(If3), \+(If4), \+(If5), \+(If6)|= is hit.

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,If6,Then6) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   call(If5)
   ->  call(Then5)
   ;   call(If6)
   ->  call(Then6)
   ;   unhandled_else_error.

%! switch(:If1,:Then1,:If2,:Then2,:If3,:Then3,:If4,:Then4,:If5,:Then5,:If6,:Then6,:Else)
%
% =switch/13=: pass 13 goals. Has _else_ case.

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,If6,Then6,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   call(If5)
   ->  call(Then5)
   ;   call(If6)
   ->  call(Then6)
   ;   call(Else).

%! if_then_else(:Condition,:Then,:Else)
%
% An implementation of the =|-> ;|= combination. Pass three goals.

if_then_else(Condition,Then,Else) :-
   call(Condition) -> call(Then) ; call(Else).

%! reify_outcome(:Condition,?SuccessThing,?FailureThing,?Out)
%
% Reification of an outcome. Pass a goal Condition and the SuccessThing to be unified with Out
% if the Goal succeeds, as well as the FailureThing to be unified with Out if the Goal fails.
% This predicate gives off the vibes of a controlled switch connecting contacts.

reify_outcome(Condition,SuccessThing,FailureThing,Out) :-
   call(Condition)
   -> (Out = SuccessThing)
   ;  (Out = FailureThing).

%! reify(:Condition,?Outcome)
%
% Reification of the outcome of goal Goal. It reifies Outcome with one of the atoms =true= or =false=.

reify(Goal,Outcome) :-
   call(Goal)
   -> (Outcome = true)
   ;  (Outcome = false).

%! if_then(:Condition,:Then)
%
% An implementation of =|->|= for which there is no "else"/"if-not-true" case.

if_then(Condition,Then) :-
   call(Condition)
   -> call(Then)
   ;  true.

%! unless(:Condition,:Else)
%
% An implementation of =|->/2|= for which there is no "if-true" case.
% This not quite the same as =|unless(Condition,Else) :- \+call(Condition) -> call(Else) ; true|=

unless(Condition,Else) :-
   call(Condition)
   -> true
   ;  call(Else).

%! unhandled_else_error
%
% Throw a non-"ISO standard" exception which is used when a switch
% hits an _else_ case and there is no _Goal_ that can be called for that
% eventuality. This is not an ISO standard exception because the "formal"
% term =programming_error= is not in the list of allowed terms (specifying
% an purportedly exclusive list of allowed terms is not a good idea).

unhandled_else_error :-
   throw(
      error(programming_error,
            context(_,"hit the unhandled 'else' case of a 'switch'"))).

%! maplist_onto_open_list(:Goal,+List,?Tip,?Fin)
%
% This is like maplist/3, but appends to the end of an open list. Initially,
% Tip is an unbound variable which becomes the _tip_ of the growing list.
% Fin is a different unbound variable which is unified with the
% final entry of last listbox of the thus constructed constructed open list.
% When done, the caller is still working with an open list and can continue
% appending at Fin.
% Note that one _could_ also pass an already instantiated difference list
% Tip-Fin instead in case of "checking".

maplist_onto_open_list(_,[],FinalFin,FinalFin) :- !.

maplist_onto_open_list(Goal,[In|MoreIn],[Out|Fin],FinalFin) :-
   call(Goal,In,Out),
   maplist_onto_open_list(Goal,MoreIn,Fin,FinalFin).

