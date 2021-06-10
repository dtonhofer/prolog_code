:- module(onepointfour_basics_space_string,
          [
           space_string/2        % space_string(?N,?String)
          ,space_string/3        % space_string(?N,?String,@Throw)
          ,space_string_smooth/2 % space_string_smooth(?N,?Stringy)
          ]).

:- use_module(library('onepointfour_basics/checks.pl')).

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

/** <module> Generate and accept strings consisting only of char 0x20, "SPACE"

This is specific to SWI-Prolog, which distinguishes "string" and "atom" as
two distinct representations of "sequences of characters".

## Examples

```
?- use_module(library('onepointfour_basics/space_string.pl')).
true.

?- space_string(10,String).
String = "          ".

?- space_string(N,"    ").
N = 4.

?- space_string(N," hey  ").
false.
```

## Running the tests

There should be a file =|space_string.plt|= nearby.
Then, if the directory of the package is on the library path:

```
?- use_module(library('onepointfour_basics/space_string.pl')).
?- load_test_files([]).
?- run_tests.
```

## History

   1. 2020-07-XX: First version.
   1. 2021-05-27: Full review.
   1. 2021-06-10: Further review.

## License

   @license [MIT License](https://opensource.org/licenses/MIT)
   @author David Tonhofer (ronerycoder@gluino.name)

## Notes: Alternatives

Another way of "generating" is:

```
length(Codes, N),
maplist(=(0'\s), Codes),    % N space character codes
string_codes(Codes, String).
```

Or even like this, which is somewhat hard to remember

```
format(string(String), '~t~*|', [2]).  % two space characters
```

However, the performance of of the first trick is much worse than the code
of this module. The format/2 trick has about the same performance.

See =|space_string_performance.plt|= for performance testing.

Another way of checking whether a string contains spaces only is:

```
string_codes(Codes, String),
maplist(=(0'\s), Codes)
```

Or even like this:

```
split_string(String, "", " ", [""]).
```

See also this code concerning "exponentiation" of an associative operation:

https://swi-prolog.discourse.group/t/power-implementation/1937
*/

%! space_string(?N,?Stringy)
%
% Succeeds iff Stringy is a string of N _SPACE_ characters.
%
% - On input, Stringy may also be an atom.
% - On output, Stringy is always a string.
%
% If both arguments are unbound, generates pairs (N,Stringy) with
% N monotonically increasing.
%
% The behaviour is _soft_: in case of bad input, the predicate
% prefers failure to throwing an exception:
%
% - Fails if N is bound to an integer but < 0 .
%
% However, errors in type (not an integer, not a string) lead to
% appropriate exceptions.

space_string(N,Stringy) :-
   space_string(N,Stringy,false).

%! space_string_smooth(?N,?Stringy)
%
% As space_string/2 but only fails, never throws.

space_string_smooth(N,Stringy) :-
   check_that(Stringy,[break(var),soft(stringy)]),
   check_that(N,[break(var),soft(int),soft(pos0int)]),
   space_string(N,Stringy,false).

%! space_string(?N,?Stringy,@Throw)
%
% As space_string/2 but whereas space_string/2 behaves _softly_
% on bad input, space_string/3 can be told to throw on the input
% problems that only elicit failure for space_string/2. For this,
% instantiate Throw to one of the atoms =|throw|= or =|true|=.

space_string(N,Stringy,Throw) :-
   nonvar(Stringy),                    % case: "accept length or determine length of 'Stringy'"
   !,
   check_that(Stringy,[hard(stringy)],Throw),
   (nonvar(N)
    ->
    check_that(N,[hard(int),tuned(pos0int)],Throw)
    ;
    true),
   atom_string(Stringy,Stringy2),      % makes sure Stringy2 is a *string representation*
   string_length(Stringy2,N),
   gen_string_of_spaces(N,Stringy2).   % regenerate N-space string for comparison with Stringy2

space_string(N,Stringy,Throw) :-
   var(Stringy),
   nonvar(N),                          % case: "generate a string"
   !,
   check_that(N,[hard(int),tuned(pos0int)],Throw),
   gen_string_of_spaces(N,Stringy).    % depending on N, this fails, but does not throw

space_string(N,Stringy,_) :-
   var(Stringy),
   var(N),                             % case: "generate pairs"
   !,
   between(0,inf,N),                   % infinite backtracking
   gen_string_of_spaces(N,Stringy).

% Generate (possibly long) strings quickly by recursively applying
% string_concat/3 on two strings of half the desired length.
% Add specific cases besides length 0 and 1 for fast generation for
% short strings.

gen_string_of_spaces( 0 ,"")           :- !.
gen_string_of_spaces( 1 ," ")          :- !.
gen_string_of_spaces( 2 ,"  ")         :- !.
gen_string_of_spaces( 3 ,"   ")        :- !.
gen_string_of_spaces( 4 ,"    ")       :- !.
gen_string_of_spaces( 5 ,"     ")      :- !.
gen_string_of_spaces( 6 ,"      ")     :- !.
gen_string_of_spaces( 7 ,"       ")    :- !.
gen_string_of_spaces( 8 ,"        ")   :- !.
gen_string_of_spaces( 9 ,"         ")  :- !.
gen_string_of_spaces(10 ,"          ") :- !.
gen_string_of_spaces(N  ,String) :-
   N >= 0,                                     % fail if N < 0
   integer(N),                                 % divmod throws on non-integer; preclude that
   divmod(N,2,Times,Remainder),
   gen_string_of_spaces(Times,S1),
   string_concat(S1,S1,S2),                    % S2 := 2*S1
   (
      Remainder>0
      ->
      gen_string_of_spaces(Remainder,SR),
      string_concat(S2,SR,String)
      ;
      String = S2
   ).

