:- module(onepointfour_basics_throwme_example,
          [
          do_stuff/1
          ]).

:- use_module(library('onepointfour_basics/throwme.pl')). 
:- use_module(library('onepointfour_basics/meta_helpers.pl')).
:- use_module(library('onepointfour_basics/safe_format.pl')).

/** <module> Example code for module "throwme.pl".

How the exception term is rendered on the toplevel may vary.
In this case, one sees a message like this:

```
ERROR: Type error: `integer' expected, found `foo' (an atom) (Expected integer but got 'foo')
       <-------->      |     <------------->   |       |     <----------------------------->
       based on        |      template text    |       |    text found in "context" subterm 
       known "formal"  |                       |  in case of "type error" 
       compound term   |                       |  the type of the second
                       |                       |  arg is printed
                       |                       |
                   first arg of           second arg of
                    "formal"               "formal"

```

Note the ISO Prolog style of the exception: It expresses
"what is expected" rather than "what has been found to be the case".

And so:

```
?- do_stuff(foo).   
ERROR: Type error: `integer' expected, found `foo' (an atom) (Expected integer but got 'foo')

?- do_stuff(-5).
ERROR: Domain error: `positive_integer' expected, found `-5' (Expected positive integer but got '-5')

?- do_stuff(2).
ERROR: Domain error: `odd_integer' expected, found `2' (Expected odd integer)

?- do_stuff(1).
Success with X = 1
true.
```

*/

%! do_stuff(+X:Integer) 
%
% Actual "business code". Call with an odd positive integer unless you
% want to generate an exception.
%
% Exception generation has been moved out to =|exc_desc/5|= clauses.

do_stuff(X) :-
   module_name(M),
   unless(integer(X) ,throwme(do_stuff_entry,expecting_integer(X),M)),
   unless(X>0        ,throwme(do_stuff_entry,expecting_positive_integer(X),M)),
   unless(odd(X)     ,throwme(do_stuff_entry,expecting_odd_integer(X),M)),
   format("Success with X = ~d~n",[X]).

odd(X) :- 
   assertion(integer(X)),
   X mod 2 =\= 0.

%! module_name(-M:Atom)
%
% The constant for the current module name

module_name(onepointfour_basics_throwme_example).

%! exc_desc(+LookupPred,+LookupTerm,-Location,-Formal,-Msg)
%
% =|exc_desc/5|= clauses are selected by throwme/2 via their first two arguments,
% which may carry additional information if they are compound. 
%
% Arguments Location (possibly a predicate indicator), Formal (the "formal term"
% of the exception term), Msg (a user-readable message) are meant to be inserted 
% into a thrown exception term. Here, the user-readable message is constructed
% dynamically, too.

exc_desc(do_stuff_entry,expecting_integer(X),
         do_stuff/1,
         type_error(integer,X),
         Msg) :- 
   safe_format("Expected integer but got '~q'",X,Msg).
                  
exc_desc(do_stuff_entry,expecting_positive_integer(X),
         do_stuff/1,
         domain_error(positive_integer,X),
         Msg) :-
   safe_format("Expected positive integer but got '~q'",X,Msg).
         
exc_desc(do_stuff_entry,expecting_odd_integer(X),
         do_stuff/1,
         domain_error(odd_integer,X),
         "Expected odd integer").
