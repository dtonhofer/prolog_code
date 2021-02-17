:- module(onepointfour_strings_stringy,
          [
          stringy/1              % is the argument an atom or a string or a number
         ,stringy_is_empty/1     % is the argument the empty atom,string or list
         ,stringy_is_chars/1     % is the argument a proper list of chars
         ,stringy_is_codes/1     % is the argument a proper list of codes (integers)
         ,stringy_is_atomic/1    % is the argument an atom or a string
         ,stringy_is_text/1      % is the argument an atom or a string or chars or codes
         ,stringy_is_anytext/1   % is the argument an atom or a string or chars or codes or a number
         ,stringy_is_anytext/2   % as stringy_is_anytext/1, but also reifies the type of argument 1 (empty list is 'empty')
          % "S" is the stringy thing made from chars in "Chars", "Want" is the atom giving the type of "S": 'atom' or 'string'
         ,stringy_chars/3        % stringy_chars(S,Chars,Want)
          % Concatenate multiple things to a single stringy thing "Sout" which shall be of type "Want" (either 'atom' or 'string')
         ,stringy_concat/10      % stringy_concat(Sin1,Sin2,Sin3,Sin4,Sin5,Sin6,Sin7,Sin8,Sout,Want)
         ,stringy_concat/9       % stringy_concat(Sin1,Sin2,Sin3,Sin4,Sin5,Sin6,Sin7,Sout,Want)
         ,stringy_concat/8       % stringy_concat(Sin1,Sin2,Sin3,Sin4,Sin5,Sin6,Sout,Want)
         ,stringy_concat/7       % stringy_concat(Sin1,Sin2,Sin3,Sin4,Sin5,Sout,Want)
         ,stringy_concat/6       % stringy_concat(Sin1,Sin2,Sin3,Sin4,Sout,Want)
         ,stringy_concat/5       % stringy_concat(Sin1,Sin2,Sin3,Sout,Want)
         ,stringy_concat/4       % stringy_concat(Sin1,Sin2,Sout,Want)
         ,stringy_concat/3       % stringy_concat(Sin1,Sout,Want)
         % Transform a thing "Sin" into a stringy thing "Sout" which shall be of type "Want" (either 'atom' or 'string')
         ,stringy_ensure/3       % string_ensure(Sin,Sout,Want)
         % Determine the "stringy length" of a thing "Sin"
         ,stringy_length/2  % string_length(Sin,Length)
         ]).

:- use_module(library('onepointfour_basics/meta_helpers.pl')).
:- use_module(library('onepointfour_basics/throwme.pl')).

/** <module> Simple predicates that try to make "manipulation of strings" and "manipulation of atoms" somewhat uniform.

## Tests

Running the tests: There should be a file =|stringy.plt|= nearby.

If you have loaded this module, a call to load_test_files/1 should run the tests:

```
?- use_module(library('onepointfour_strings/stringy.pl')).
?- load_test_files([]).
?- run_tests.
```

## History

- 2021-01-19: Reviewed
- 2021-02-16: Reviewed for packs, updated to pldoc.
*/

%! stringy(+X) Stringy type test.
%
% Succeeds if X is something "stringy", fails otherwise. An unbound variables
% leads to failure. Stringy things are:
%
%    - atoms
%    - SWI-Prolog strings 
%    - a number is accepted too, as it is "transformable into a string" by
%      atom_string/2 (which leads to the question: "how exactly
%      is a float or a rational transformed into a string?". The answer is:
%      it depends!
%
% Compare with must_be/2 which knows of the following types:
%
%    - chars  : list of 1-character atoms; includes the empty list
%    - codes  : list of integers >= 0; includes the empty list
%    - string : passes string/1, an SWI-Prolog string
%    - atom   : passes atom/1
%    - text   : atom or string or chars or codes (but not numbers even though some predicates "textify" those)
%
% @bug It is difficult to judge whether numbers or event lists of chars or lists of numbers should be in this set.

stringy(X) :-
   (atom(X)
    ;
    string(X)
    ;
    number(X)),!. % terminal ! to make this deterministic


%! stringy_is_empty(+X) Succeed exactly if X is "empty": The empty string, the empty atom but also the empty list.
% 
% @bug It is difficult to judge whether the empty list should be in this set. If the empty list is in here,
% should stringy/1 succeed for lists?

stringy_is_empty(X) :- 
   (X==''
    ;
    X==""
    ;
    X==[]),!.  % terminal ! to make this deterministic

IS: Certainly is
IS_NOT: Certainly is not
CAN_BE: Looks like but future instantiation can still change this
Attribute: EMPTY

                                                                                  

                                               +---- openvarlist?: nonempty open list of >= 1 var
                                               |                                       
                             +--- openlist? ---+---- opencharlist?: nonempty open list of >= 1 char, may contain vars
                             |                 |                                       
                             |                 +---- opencodelist?: nonempty open list of >= 1 unicode code points, may contain vars
                             |
                             |                                                         +--- charlist! nonempty list of only chars ("characters", atoms of length 1)
                             |                                                         |
                             |                                       +---- charlist ---+
                             |                                       |                 |   
                             |                                       |                 +--- charlist? nonempty list of at least 1 char and at least 1 var
                             |                                       |
                             |                                       |                 +--- codelist! nonempty list of only unicode code points
                             |                                       |                 |
                             |                                       +---- codelist ---+
                             |                                       |                 |   
                             |                                       |                 +--- codelist? nonempty list of at least 1 unicode code points and at least 1 var
                             |                       +--- list ------+                                                                                  
                             |                       |               |          
                             |                       |               +---- varlist?: nonempty list of only vars; edge case
                             |                       |               |
                             |                       |               +---- emptylist?: empty list; edge case
                   texty? ---+             +---text--+           
                             |             |         |          
                             |             |         |               +-- atom! (including the empty atom)          
                             |             |         |               |          
                             +---anytext---+         +--- stringy! --+
                             |             |                         |          
                             |             |                         +-- string! (SWI-Prolog strings including the empty string)          
                             |             |          
                             |             +-- number!: acceptable because a number can be           
                             |                          transformed into text (according to some           
                             |                          unspecified convention...)
                             |                       
                             +--- var? (could be anything)

success
 settled : it is texty and will stay texty (actually, it is in the category and will stay in the category)
 free:     it is text now but may become nontexty depending on future instantiations
failure
 it's not texty

attributes
 
 empty     if it is a closed list, it is empty, if it is an atom, it is the empty atom, if it is a string, it is the empty string
 length(N) if it is a list, it has length N (even for an open list)
 vars(N)   if it is a list, it has N vars
 open      if it is a list, it's an open list
 closed    if it is a list, it's a closed list
 
category
   
%! textycat(@X,?Cat,?Surety,?Attributes)

textycat(X,C) :- nonvar(C), textycat(X,CC),isa(CC,C).

textycat(X,var)    :- var(X),!.
textycat(X,string) :- string(X),!.
textycat(X,atom)   :- atom(X),!.

validcat(C) :-


isa(SubCat,SuperCat) :- parent(SubCat,SuperCat).
isa(SubCat,SuperCat) :- parent(SubCat,NextCat),isa(NextCat,SuperCat).

parent(var,texty).
parent(anytext,texty).
parent(openlist,texty).
parent(number,anytext).
parent(text,anytext).
parent(list,text).
parent(stringy,text).
parent(atom,stringy).
parent(string,stringy).
parent(charlist,list).
parent(codelist,list).
parent(varlist,list).
parent(emptylist,list).
parent(openvarlist,openlist).
parent(opencharlist,openlist).
parent(opencodelist,openlist).

canbe(var).
canbe(varlist).
canbe(emptylist).
canbe(openlist).








attributes: canbe, empty, vars(N) in lists length(N) in lists

any-var   -> can be
any-anytext-number
any-anytext-text-stringy-atom  maybe empty
any-anytext-text-stringy-string  maybe empty
any-anytext-text-list-chars      maybe can be if there are some vars
any-anytext-text-list-codes      maybe can be if there are some vars
any-anytext-text-list-vars       -> can be
any-anytext-text-list-empty      -> can be
any-anytext-openlist-vars        -> can be
any-anytext-openlist-chars       -> can be
any-anytext-openlist-codes       -> can be

relationship compatible with 
anytext
any
stringy
text
list











                             
                             



          
%! stringy_is_anytext(X)
%
% Succeed if X is "anytext" (which is stuff that can be processed by
% SWI-Prolog string manipulation predicates): atom or string or chars or codes or number.
% The string_is_anytext/2 reifies the type: 'atom', 'string', 'number', 'chars', 'codes', 'empty'
% ===

stringy_is_anytext(X) :-
   stringy_is_anytext(X,_).

stringy_is_anytext(X,What) :-
   switch(
      X==[]               , (What=empty), % inherently ambiguous
      atom(X)             , (What=atom), % no special distinction for the empty atom
      string(X)           , (What=string), % no special distinction for the empty string
      number(X)           , (What=number),
      stringy_is_chars(X) , (What=chars),
      stringy_is_codes(X) , (What=codes),
      fail),!.  % terminal ! to make this deterministic

% ===
% Succeed if X is something "text stringy" (the understanding for "text" from must_be/2)
% text : atom or string or chars or codes (but not numbers)
% ===

stringy_is_text(X) :-
   (stringy_is_atomic(X)
    ;
    stringy_is_chars(X)
    ;
    stringy_is_codes(X)),!. % terminal ! to make this deterministic

% ===
% Succeed if X is something "atomic stringy":
% - an atom
% - a string
% ===

stringy_is_atomic(X) :-
   (atom(X)
    ;
    string(X)),!. % terminal ! to make this deterministic

% ===
% Succeed if X is a list of chars. We zip down the whole list.
% This does not accept unbound variables and open lists.
% ===

stringy_is_chars(X) :-
   switch(
      var(X),    fail,
      X==[],     true,
      X=[C|Cs],  (atom(C),atom_length(C,1),stringy_is_chars(Cs)),
      fail).

% ===
% Succeed if X is a list of codes. We zip down the whole list.
% This does not accept unbound variables and open lists.
% ===

stringy_is_codes(X) :-
   switch(
      var(X),    fail,
      X==[],     true,
      X=[C|Cs],  (integer(C),between(0,0x10FFFF,C),stringy_is_codes(Cs)),
      fail).

% ===
% Transform a stringy thing (actually, a less-than-stringy thing: only atoms or
% strings are accpet) into a list of chars (atoms of length 1).

% This is not needed as atom_length/2 and string_length/2 work for both strings
% and atoms, but it removes specificity of using atom_chars/2 or string_chars/2
% from the program text.
%
% This works both in the following directions:
%
% "S" is given and a string or atom. Then "Chars" is unified with the result of
% splitting and "Want" is unified with one of the atoms 'atom' or 'string'.
% Additionally, if "S" is not a string or atom, "Want" is unified with 'other'
% and "S" is broken apart with string_chars/2. using 'other' does not work
% in the direction "Chars" --> "String"
%
% "Chars" is given and "Want" is one of the atoms 'atom' or 'string'. Then
% "S" is unified with the result of concatenation and it will have the type
% given by "Want".
% If "Chars" is given and "Want" is unbound, then it is unified with 'string'
% and a string is created from "Chars".
% In this mode, "Chars" can also be a list of integers, which will be
% interpreted as codes!
% ===

stringy_chars(S,Chars,Want) :-
   switch(
      stringy(S)  , stringy_chars_when_S_is_stringy(S,Chars,Want),
      var(S)      , stringy_chars_when_S_is_var(S,Chars,Want),
      type_error(_,_)).                         % TODO: Fix exception

stringy_chars_when_S_is_stringy(S,Chars,Want) :-
   switch(
      atom(S)   , (Want=atom,atom_chars(S,Chars)),
      string(S) , (Want=string,string_chars(S,Chars)),
      (Want=other,string_chars(S,Chars))).      % Not sure of the type; try string_chars/2

stringy_chars_when_S_is_var(S,Chars,Want) :-
   switch(
      (Want==atom)   , atom_chars(S,Chars),     % leave accepting/generating to the builtin
      (Want==string) , string_chars(S,Chars),   % leave accepting/generating to the builtin
      (Want=string)  , string_chars(S,Chars),   % if "Want" is var, default to string
      domain_error(_,_)).                       % TODO: Fix exception

% ===
% Concatenate mode: concatenate multiple stringy things into a final one
% which shall have the type given by "Want" (either 'string' or 'atom').
% Anything that is accepted by string_concat/3 is accepted (unbound
% variables are not accepted)
% ===

stringy_concat(Sin1,Sin2,Sin3,Sin4,Sin5,Sin6,Sin7,Sout,Want) :-      stringy_concat(Sin1,Sin2,Sin3,Sin4,Sin5,Sin6,Sin7,''  ,Sout,Want).
stringy_concat(Sin1,Sin2,Sin3,Sin4,Sin5,Sin6,Sout,Want) :-           stringy_concat(Sin1,Sin2,Sin3,Sin4,Sin5,Sin6,''  ,''  ,Sout,Want).
stringy_concat(Sin1,Sin2,Sin3,Sin4,Sin5,Sout,Want) :-                stringy_concat(Sin1,Sin2,Sin3,Sin4,Sin5,''  ,''  ,''  ,Sout,Want).
stringy_concat(Sin1,Sin2,Sin3,Sin4,Sout,Want) :-                     stringy_concat(Sin1,Sin2,Sin3,Sin4,''  ,''  ,''  ,''  ,Sout,Want).
stringy_concat(Sin1,Sin2,Sin3,Sout,Want) :-                          stringy_concat(Sin1,Sin2,Sin3,''  ,''  ,''  ,''  ,''  ,Sout,Want).
stringy_concat(Sin1,Sin2,Sout,Want) :-                               stringy_concat(Sin1,Sin2,''  ,''  ,''  ,''  ,''  ,''  ,Sout,Want).
stringy_concat(Sin1,Sout,Want) :-                                    stringy_concat(Sin1,'',  ''  ,''  ,''  ,''  ,''  ,''  ,Sout,Want).

stringy_concat(Sin1,Sin2,Sin3,Sin4,Sin5,Sin6,Sin7,Sin8,Sout,Want) :-
   unless(atom(Want),type_error(_,_)),          % 'Want' must be a given. TODO: Fix exception
   if_then(                                     % If 'Sout' is a given, its type must match 'Want' or we fail at once
      nonvar(Sout),
      unless(
         ((Want==atom,atom(Sout));(Want==string,string(Sout))),
         fail
      )),
   stringy_concat_pair_to_string_kickoff(Sin1,Sin2,Mid1),
   stringy_concat_pair_to_string(Mid1,Sin3,Mid2),
   stringy_concat_pair_to_string(Mid2,Sin4,Mid3),
   stringy_concat_pair_to_string(Mid3,Sin5,Mid4),
   stringy_concat_pair_to_string(Mid4,Sin6,Mid5),
   stringy_concat_pair_to_string(Mid5,Sin7,Mid6),
   stringy_concat_pair_to_string(Mid6,Sin8,SoutTmp),
   assertion(string(SoutTmp)),
   stringy_ensure(SoutTmp,Sout,Want).

stringy_concat_pair_to_string_kickoff(Sin1,Sin2,Sout) :-
   unless(nonvar(Sin1),instantiation_error(_)),  % "Sin1" must be a given. TODO: Fix exception
   unless(nonvar(Sin2),instantiation_error(_)),  % "Sin2" must be a given. TODO: Fix exception
   string_concat(Sin1,Sin2,Sout).                % "Sout" is not a nonvar string for sure (maybe empty)

stringy_concat_pair_to_string("","","")    :- !. % "Sin" is the empty string, to be concatenated with empty: trivial
stringy_concat_pair_to_string("",'',"")    :- !. % "Sin" is the empty string, to be concatenated with empty: trivial
stringy_concat_pair_to_string(Sin,'',Sin)  :- !. % "Sin" is known to be String (and nonempty), pass it on
stringy_concat_pair_to_string(Sin,"",Sin)  :- !. % "Sin" is known to be String (and nonempty), pass it on
stringy_concat_pair_to_string("",Sin,Sout) :-    % Sin may be anything, transform before passing it on
   !,
   stringy_ensure(Sin,Sout,string).

stringy_concat_pair_to_string(Sin1,Sin2,Sout) :-
   unless(nonvar(Sin2),instantiation_error(_)),  % 'Sin2' must be a given. TODO: Fix exception
   string_concat(Sin1,Sin2,Sout).                % this transforms arbitrary transformations on Sin2, but yields a string

% ===
% Transform a stringy thing "Sin" into the stringy thing "Sout", which shall have
% the type given by "Want": either atom or string.
%
% "Want" must be one of: atom, string. Anything else raises an exception.
%
% "Sin"  must be instantiated to something that "atom_string" accepts. An
%        exception is raised on non-instantiation. Numbers, lists of chars
%        and lists of codes and evidently atoms and strings are accepted.
%        This predicate is "functional/one-way"!
%
% This also works in "accepting" mode, whereby at call time "Sout" is
% already instantiated to a stringy thing that matches the type given by
% "Want".
% ===

stringy_ensure(Sin,Sout,Want) :-
   unless(atom(Want),type_error(_,_)),          % 'Want' must be a given. TODO: Fix exception
   unless(nonvar(Sin),instantiation_error(_)),  % 'Sin' must be a given. TODO: Fix exception
   if_then(                                     % If 'Sout' is a given, its type must match 'Want' or we fail at once
      nonvar(Sout),
      unless(
         ((Want==atom,atom(Sout));(Want==string,string(Sout))),
         fail
      )),
   switch(
      (Want==atom)   , atom_string(Sout,Sin),   % leave accepting/generating to the builtin
      (Want==string) , atom_string(Sin,Sout),   % leave accepting/generating to the builtin
      domain_error(_,_)).                       % TODO: Fix exception


%! stringy_length(+Sin:Text,?Length:Integer)
%
% Determine the length of a _stringy thing_ Sin, which may be an atom,
% a string or some round term that can be transformed into something that
% has a "length".
%
% This is not really needed a atom_length/2 and string_length/2 work for
% both strings and atoms, but it removes the specificity of using the call
% to atom_length/2 or string_length/2 from program text.
%
% This also works in "accepting" mode. It succeeds if at call time Length
% is already instantiated to a valid length that matches the length of the
% stringy thing. A negative Length causes failure, not an exception.
% ===

stringy_length(Sin,Length) :-
   unless(nonvar(Sin),instantiation_error(_)),  % TODO: Fix exception
   switch(
      atom(Sin),   atom_length(Sin,Length),     % arg checking & throwing is left to atom_length/2
      string(Sin), string_length(Sin,Length),   % arg checking & throwing is left to to string_length/2
      ground(Sin), string_length(Sin,Length),   % neither an atom nor a string; arbitrarily pass this to string_length/2
      domain_error(_,Sin)).                     % TODO. Fix exception

      
