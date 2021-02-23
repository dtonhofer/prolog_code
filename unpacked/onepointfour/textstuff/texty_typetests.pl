:- module(onepointfour_texty_typetests,
          [
           texty/2                 % texty(@X,-TypeDesc)
          ,texty_typedesc/1        % texty_typedesc(@TypeDesc)
          ,texty_subsumes/2        % texty_subsumes(@TypeDescA,@TypeDescN)
          ,texty_unadorn/2         % texty_unadorn(@TypeDescIn,-TypeDescOut)
          ,texty_leaf/1            % texty_leaf(@TypeDesc)
          ,texty_leaf/2            % texty_leaf(@TypeDesc,-Outcome)
          ,texty_settled/1         % texty_settled(@TypeDesc)
          ,texty_settled/2         % texty_settled(@TypeDesc,-Outcome)
          ]).
          
/** <module> Type testing of "texty terms"

This module implements type tests of "texty terms", i.e. terms that might or can
be interpreted as text, or else certainly _are_ text..

The type tree for "texty terms" is as follows:

```
           +--- var?: an unbound variable; can become anything
           |
           |
           |                   +--- openvarlist?: nonempty open list of > 0 vars
           |                   |
           +--- openlist? -----+--- opencharylist?: nonempty open list of > 0 char, may contain vars
           |                   |
           |                   +--- opencodeylist?: nonempty open list of > 0 unicode code points, may contain vars
           |
           |                   +--- varlist?: list of N > 0 vars; edge case
           |                   |
           +--- changylist? ---+--- charylist?: list of C > 0 chars and V > 0 vars
           |                   |
           |                   +--- codeylist?: list of C> 0 unicode code points and V > 0 vars
           |
           |                                               +--- emptylist!: empty list; edge case as not sure whether charlist or codelist
           |                                               |
           |                             +--- textlist! ---+--- charlist!: nonempty list of chars (atoms of length 1)
           |                             |                 |
           |                             |                 +--- codelist!: nonempty list of unicode code points (integers between 0 and 0x10FFFF)
           |                             |
 texty* ---+               +--- text! ---+
           |               |             |
           |               |             |                +--- atom!: Prolog atoms including the empty atom '' (but not the empty list [])
           |               |             |                |
           +---anytext! ---+             +--- stringy! ---+
                           |                              |
                           |                              +--- string!: SWI-Prolog strings including the empty string ""
                           |
                           +--- number!  acceptable because a number can be
                                         transformed into text (according to some
                                         unspecified convention...)
```

The subtree rooted at _anytext_ contains those terms accepted by SWI-Prolog text-handling predicates. 
See the SWI-Prolog manual page [Predicates that operate on strings](https://eu.swi-prolog.org/pldoc/man?section=string-predicates).

The following vocabulary holds:

| _var_                  | an unbound variable.  |
| _list_, _proper list_  | a list that is properly terminated =|[a,b,c]|= alias =|[a|[b|[c|[]]]]|=. |
| _open list_            | a list that has a _var_ at its _fin_ (_fin_ being the second position of the last listbox in the list). Example: =|[a,b,c|_]|=. It may become a _proper list_ by an appropriate instantiation. Generally the concept of _open list_ includes a simple _var_ but that case is handled separately here: _open list_ is understood to always be non- _var_. |
| _code_                 | an integer [unicode code point](https://en.wikipedia.org/wiki/Unicode#Code_planes_and_blocks) between 0 and 0x10FFFF. Wwe don't bother to verify that the code point indeed falls into one of the allowed codepages. |
| _char_, _character_    | a Prolog atom of length 1.  |
| _string_               | an SWI-Prolog string. An empty string and a empty atom are different things. |
| _number_               | any of the SWI-Prolog number types (see number/1): integers, non-integer rationals or floats. |                        
| _anytext_              | "texty terms" that can be processed by SWI-Prolog predicates like atom_length/2, string_length/2 etc.  |

The remainder of the vocabulary, =|stringy|=, =|textlist|=, =|text|=, =|emptylist|=, =|charlist|=, =|codelist|= etc. can be deduced from the type tree.

The =|?|=, =|!|= and =|*|= are annotations about how "settled" the categorization as a "texty term" is:

| =|?|=  | the term is "texty" but this categorization may change depending on future instantiations: its "texty-ness" may be destroyed. Alternatively instantiations may keep "texty-ness" and move a term "downwards" in the tree, for example a _var_ may become an _openvarlist_ which may become a _varlist_ which which may become a _codeylist_ which may become a _codelist_ |
| =|!|=  | the term is "texty" and that will stay so for sure ("texty-ness" is "settled"). Actually, the term is ground. | 
| =|X|=  | only for non-leaf types: subtypes exist which are =|?|= and others which are =|!|=. (FIXME: the doc server does not accept * at the place of X) |

## Load module with

```
?- use_module(library('onepointfour_texty/texty_typetests.pl')).
```

## History

   1. 2021-02-17: First version based on now-discarded older code from mid-2020.

## More

   @license [Zero-Clause BSD / Free Public License 1.0.0 (0BSD)](https://opensource.org/licenses/0BSD)
   @author David Tonhofer (ronerycoder@gluino.name)

*/

%! texty(@X,-TypeDesc)
%
% Determine the "texty" type descriptor TypeDesc applicable to X.
% 
% **Succeed** if X can be interpreted as a "texty term", the type
% descriptor for X can be unified with TypeDesc.
%
% **Fail** if X cannot be interpreted as a "texty term" (e.g. if it is a compound term 
% that is not a listbox).
%
% No exceptions are thrown.
%
% Type descriptors are as follows. If they are compound terms, they are called
% "adorned". In that case, there is an unadorned version of the type descriptor, too.
%
% | =|var|=                        | var   |
% | =|emptylist|=                  | empty list   |
% | =|string(L)|=                  | string of length L >= 0  |
% | =|atom(L)|=                    | atom of length L >= 0  |
% | =|charlist(N)|=                | proper list with N > 0 chars   |
% | =|codelist(N)|=                | proper list with N > 0 codes   |
% | =|varlist(N)|=                 | proper list with N > 0 vars   |
% | =|charylist(N,V,L)|=           | proper list with N > 0 chars, V > 0 vars, length L = N+V  |
% | =|codeylist(N,V,L)|=           | proper list with N > 0 codes, V > 0 vars, length L = N+V  |
% | =|openvarlist(N)|=             | open list with N > 0 vars   |     
% | =|opencharylist(N,V,L)|=       | open list with N > 0 chars, V >= 0 vars, length L = N+V   |
% | =|opencodeylist(N,V,L)|=       | open list with N > 0 codes, V >= 0 vars, length L = N+V   |
%
% Derived:
%
% | =|stringy(L)|=                  | string or atom of length L >= 0  |
% | =|textlist(N)|=                 | list of length L >= 0  |
% | =|text(M)|=                     | text of length M >= 0  |
% | =|anytext|=                     | anytext  |
% | =|texty|=                       | texty  |
%
% ## Edge cases
%
% =|texty(X,X)|= fails, which is logically correct.
%
% However, 
% 
% =|texty([X,Y,X,Z],Y).|= yields =|Y = varlist(4).|= which is **not** logically correct.
% 
% In fact, one should preclude variable sharing between the first and second argument.
% Or test that the type descriptor is still valid after unification with the second
% argument. That may be too expensive for what it brings to the table.
% 
% Another case of interest:
% 
% =|texty([X,X,X|X],K).|= yields =|K = openvarlist(3).|= 
%
% The above cannot be true because X must be a list, but a list cannot appear
% in any "element position". 

texty(X,var) :- var(X),!.  
   
texty([],emptylist) :- !.
   
texty([X|Xs],TypeIdCompound) :-
   !,
   listy([X|Xs],0,0,0,VarCount,CharCount,CodeCount,TypeFromListy),
   decision(TypeFromListy,VarCount,CharCount,CodeCount,TypeIdCompound).
   
texty(X,string(Length)) :- string(X),!,string_length(X,Length).
   
texty(X,atom(Length)) :- atom(X),!,atom_length(X,Length).
   
texty(X,number) :- number(X).

%! decision(+TypeFromListy,+VarCount,+CharCount,+CodeCount,-TypeIdCompound)
%
% Decision on output values after a list has been examined. Just match what was
% found in the list and set outputs accordingly. Note that in no cases can the examined
% list have been the empty list because that case is special.

decision(properlist , 0        , 0         , CodeCount , codelist(CodeCount))                      :- !,assertion(CodeCount > 0).
decision(properlist , 0        , CharCount , 0         , charlist(CharCount))                      :- !,assertion(CharCount > 0).
decision(properlist , VarCount , 0         , 0         , varlist(VarCount))                        :- !,assertion(VarCount > 0).
decision(properlist , VarCount , 0         , CodeCount , codeylist(CodeCount,VarCount,Length))     :- !,assertion(CodeCount > 0),assertion(VarCount > 0),Length is CodeCount+VarCount.
decision(properlist , VarCount , CharCount , 0         , charylist(CharCount,VarCount,Length))     :- !,assertion(CharCount > 0),assertion(VarCount > 0),Length is CharCount+VarCount.
decision(openlist   , VarCount , 0         , 0         , openvarlist(VarCount))                    :- !,assertion(VarCount > 0).
decision(openlist   , VarCount , 0         , CodeCount , opencodeylist(CodeCount,VarCount,Length)) :- !,assertion(CodeCount > 0),Length is CodeCount+VarCount.
decision(openlist ,   VarCount , CharCount , 0         , opencharylist(CharCount,VarCount,Length)) :- !,assertion(CharCount > 0),Length is CharCount+VarCount.

%! counterupdate(@X,+VarCountIn,+CharCountIn,+CodeCountIn,-VarCountOut,-CharCountOut,-CodeCountOut)
%
% Examine element X of a list and increment the corresponding counter of chars, codes or vars.
% Fail if X is not a char, code or var, or if it is a char and codes have already been found
% or if it is a code and chars have already been found.

counterupdate(X,VarCountIn,CharCount,CodeCount,VarCountOut,CharCount,CodeCount) :-
   var(X),
   !,
   VarCountOut is VarCountIn+1.
   
counterupdate(X,VarCount,CharCountIn,CodeCount,VarCount,CharCountOut,CodeCount) :-
   char(X),
   !,
   (CodeCount == 0), % Fail at once if both char and code found
   CharCountOut is CharCountIn+1.
   
counterupdate(X,VarCount,CharCount,CodeCountIn,VarCount,CharCount,CodeCountOut) :-
   unicode(X),
   !,
   (CharCount == 0), % Fail at once if both char and code found
   CodeCountOut is CodeCountIn+1.
   
%! unicode(@X) 
%
% Is X a valid unicode code point (but not a var)?
%
% We do not fully & faithfully translate the valid Unicode codepages into this test...

unicode(X) :- integer(X), X >= 0, X =< 0x10FFFF.

%! char(@X) 
%
% Is X a valid char (but not a var)

char(X) :- atom(X),atom_length(X,1).
      
%! listy(@List,+VarCountIn,+CharCountIn,+CodeCountIn,-VarCountOut,-CharCountOut,-CodeCountOut,-Type)
% 
% Going down something that looks like a list, updating counters and setting Type. Fails if this is 
% neither an open list nor a proper list, or if it contains other things than vars, chars or codes.
% Additionally, chars and codes must not be mixed.

% Case: Second element of listbox is a var: this is an open list, but we still need to examine X. Type := openlist.

listy([X|Xs],VarCountIn,CharCountIn,CodeCountIn,VarCountOut,CharCountOut,CodeCountOut,openlist) :-
   var(Xs),
   !,
   counterupdate(X,VarCountIn,CharCountIn,CodeCountIn,VarCountOut,CharCountOut,CodeCountOut).
   
% Case: Still looks like a list from here, continue down the list.

listy([X1|[X2|Xs]],VarCountIn,CharCountIn,CodeCountIn,VarCountOut,CharCountOut,CodeCountOut,Type) :-
   !,
   counterupdate(X1,VarCountIn,CharCountIn,CodeCountIn,VarCountMid,CharCountMid,CodeCountMid),
   listy([X2|Xs],VarCountMid,CharCountMid,CodeCountMid,VarCountOut,CharCountOut,CodeCountOut,Type).
      
% Case: End of proper list. Type := properlist

listy([X],VarCountIn,CharCountIn,CodeCountIn,VarCountOut,CharCountOut,CodeCountOut,properlist) :-
   !,
   counterupdate(X,VarCountIn,CharCountIn,CodeCountIn,VarCountOut,CharCountOut,CodeCountOut).   
   
% =============================================================================

%! texty_unadorn(@TypeDescIn,-TypeDescOut)
%
% Drop any adornments. Basically, unify the second argument with the
% functor name.
%
% The following prints both versions:
%
% ```
% texty_typedesc(X),texty_unadorn(X,Y),format("possibly adorned: ~q, unadorned: ~q~n",[X,Y]).
% ```
%
% Throws assertion error if any of the passed TypeDescIn is invalid according to
% texty_typedesc/1 (unless assertions have been disabled).

texty_unadorn(TypeDescIn,TypeDescOut) :-
   assertion(nonvar(TypeDescIn)),
   assertion(texty_typedesc(TypeDescIn)),
   (atom(TypeDescIn) -> TypeDescOut=TypeDescIn ; compound_name_arity(TypeDescIn,TypeDescOut,_)).

% =============================================================================

%! texty_subsumes(@SuperTypeDesc,@SubTypeDesc)
%
% Find out whether SuperTypeDesc subsumes SubTypeDesc. Works for adorned and
% unadorned TypeDesc.
% 
% Throws assertion error if any of the passed TypeDesc is invalid according to
% texty_typedesc/1 (unless assertions have been disabled).
%
% N.B. This relation is not reflexive. 
%
% Examples:
%
% Print all the relationships:
%
% ```
% texty_subsumes(X,Y),format("~q << ~q~n",[X,Y]).
% ```
%
% Due to nondeterminism and the dropping of adornments, there may be several solutions:
%
% =|stringy << atom(4)]=
%
% ```
% ?- texty_subsumes(stringy,atom(4)).
% true ;
% false.
% ```
%
% =|text << stringy << atom(4)]= or % =|text << stringy(4) << atom(4)]=
%
% ```
% ?- texty_subsumes(text,atom(4)).
% true ;
% true ;
% false.
% ```
%
% =|text(4) << stringy(4) << atom(4)]=
%
% ```
% ?- texty_subsumes(text(4),atom(4)).
% true ;
% false.
% ```

texty_subsumes(A,B) :-
   (nonvar(A) -> assertion(texty_typedesc(A)) ; true),
   (nonvar(B) -> assertion(texty_typedesc(B)) ; true),
   texty_subsumes_unguarded(A,B).

texty_subsumes_unguarded(A,B) :- texty_subsumes_directly(A,B).
   texty_subsumes_unguarded(A,B) :- texty_subsumes_directly(A,T),texty_subsumes_unguarded(T,B).

texty_subsumes_directly(texty,var).
texty_subsumes_directly(texty,openlist).
texty_subsumes_directly(texty,changylist).
texty_subsumes_directly(texty,anytext).

texty_subsumes_directly(openlist,openvarlist).
texty_subsumes_directly(openlist,opencharylist).
texty_subsumes_directly(openlist,opencodeylist).

texty_subsumes_directly(openlist,openvarlist(_)).
texty_subsumes_directly(openlist,opencharylist(_,_,_)).
texty_subsumes_directly(openlist,opencodeylist(_,_,_)).

texty_subsumes_directly(changylist,varlist).
texty_subsumes_directly(changylist,charylist).
texty_subsumes_directly(changylist,codeylist).

texty_subsumes_directly(changylist,varlist(_)).
texty_subsumes_directly(changylist,charylist(_,_,_)).
texty_subsumes_directly(changylist,codeylist(_,_,_)).

texty_subsumes_directly(anytext,number).

texty_subsumes_directly(anytext,text).
texty_subsumes_directly(anytext,text(_)).

texty_subsumes_directly(text,textlist).
texty_subsumes_directly(text,stringy).

texty_subsumes_directly(text(L),textlist(L)).
texty_subsumes_directly(text(L),stringy(L)).

texty_subsumes_directly(text,textlist(_)).
texty_subsumes_directly(text,stringy(_)).

texty_subsumes_directly(stringy,atom).
texty_subsumes_directly(stringy,string).

texty_subsumes_directly(stringy(L),atom(L)).
texty_subsumes_directly(stringy(L),string(L)).

texty_subsumes_directly(stringy,atom(_)).
texty_subsumes_directly(stringy,string(_)).

texty_subsumes_directly(textlist,emptylist).
texty_subsumes_directly(textlist,charlist).
texty_subsumes_directly(textlist,codelist).

texty_subsumes_directly(textlist(0),emptylist).
texty_subsumes_directly(textlist(L),charlist(L)).
texty_subsumes_directly(textlist(L),codelist(L)).

texty_subsumes_directly(textlist,charlist(_)).
texty_subsumes_directly(textlist,codelist(_)).

% =============================================================================

%! texty_typedesc(@TypeDesc)
%
% Accept valid TypeDesc. texty_typedesc/1 accepts both the adorned (compound term
% with additional information beyond the functor name) and unadorned (just an atom)
% forms. If adorned, additional checks are made on the functor arguments.
%
% To enumerate all typedesc, run `texty_typedesc(TD).` It generates both the
% adorend and unadorned forms.
%
% Examples:
%
% Both of these are accepted:
%
% ```
% ?- texty_typedesc(varlist).
% true.
% 
% ?- texty_typedesc(varlist(10)).
% true.
% ```
%
% But not this one, which is typedesc adorned with an invalid length:
%
% ```
% ?- texty_typedesc(varlist(-1)).
% false.
% ```

% Always unadorned

texty_typedesc(texty).
texty_typedesc(anytext).
texty_typedesc(var).
texty_typedesc(openlist).
texty_typedesc(changylist).
texty_typedesc(number).
texty_typedesc(emptylist).

% "text" can have a length L >= 0

texty_typedesc(text(L)) :- var_or_positive(L).  
texty_typedesc(text).  

% "stringy" can have a length L >= 0

texty_typedesc(stringy(L)) :- var_or_positive(L).
texty_typedesc(stringy).

% "textlist" can have a length L >= 0

texty_typedesc(textlist(L)) :- var_or_positive(L).  
texty_typedesc(textlist).  

% "charlist"/"codelist" can have a length L > 0

texty_typedesc(charlist).   
texty_typedesc(codelist). 
texty_typedesc(charlist(L)) :- var_or_strictly_positive(L).
texty_typedesc(codelist(L)) :- var_or_strictly_positive(L).

% "atom"/"string" can have a length L >= 0

texty_typedesc(atom).
texty_typedesc(string).
texty_typedesc(atom(L)) :- var_or_positive(L).
texty_typedesc(string(L)) :- var_or_positive(L).

% "openvarlist" can have a length L > 0

texty_typedesc(openvarlist(L)) :- var_or_strictly_positive(L).
texty_typedesc(openvarlist).

% "opencharylist"/"opencodeylist" are bit complex

texty_typedesc(opencharylist(N,V,L)) :- var_or_valid_triple(N,V,L).
texty_typedesc(opencharylist).

texty_typedesc(opencodeylist(N,V,L)) :- var_or_valid_triple(N,V,L).
texty_typedesc(opencodeylist).

% "varlist" can have a length > 0

texty_typedesc(varlist(L)) :- var_or_strictly_positive(L).
texty_typedesc(varlist).

% "charylist"/"codeylist" are bit complex

texty_typedesc(charylist(N,V,L)) :- var_or_valid_strict_triple(N,V,L).
texty_typedesc(charylist).

texty_typedesc(codeylist(N,V,L)) :- var_or_valid_strict_triple(N,V,L).
texty_typedesc(codeylist).

% helpers for texty_typedesc/1

var_or_positive(L) :- var(L);(integer(L),L>=0).
var_or_strictly_positive(L) :- var(L);(integer(L),L>0).
var_or_valid_triple(N,V,L) :-
   (var(N);(integer(N),N>0)),
   (var(V);(integer(V),V>=0)),
   (var(L);(integer(L),L>0)),
   ((nonvar(N),nonvar(V),nonvar(L)) -> (N+V =:= L) ; true).  

var_or_valid_strict_triple(N,V,L) :-
   (var(N);(integer(N),N>0)),
   (var(V);(integer(V),V>0)),
   (var(L);(integer(L),L>0)),
   ((nonvar(N),nonvar(V),nonvar(L)) -> (N+V =:= L) ; true).  

% =============================================================================

%! texty_leaf(@TypeDesc)
%
% Accept valid TypeDesc that is a leaf typedesc. 
% texty_leaf/1 accepts both the adorned (compound term) form and the
% unadorned form.
%
% Throws assertion error if any of the passed TypeDesc is invalid according to
% texty_typedesc/1 (unless assertions have been disabled).

texty_leaf(TypeDesc) :-
   (nonvar(TypeDesc) -> assertion(texty_typedesc(TypeDesc)) ; true),
   leaf(TypeDesc).
   
% =============================================================================

%! texty_leaf(@TypeDesc,-Outcome)
%
% Reified vesion of texty_leaf/1 which unifies Outcome to either =|inner|=
% or =|leaf|=.

texty_leaf(TypeDesc,What) :-
   assertion(var(What);What==leaf;What==inner),
   texty_leaf_2(TypeDesc,What).

% The following is designed to permit enumeration:

texty_leaf_2(TypeDesc,leaf) :- texty_leaf_2(TypeDesc). % fast for enum and test
texty_leaf_2(TypeDesc,inner) :- texty_typedesc(TypeDesc),\+texty_leaf_2(TypeDesc). % possible to enumerate and test

% ---
% Helper for leaf decision. Instead of having clauses for adorned
% and unadorned type descriptors, one could also "unadorn" a type
% descriptor. But this seems good enough. Might even be faster.
% ---

leaf(var).
leaf(number).
leaf(emptylist).
leaf(charlist).
leaf(codelist).
leaf(charlist(_)).
leaf(codelist(_)).
leaf(atom).
leaf(string).
leaf(atom(_)).
leaf(string(_)).
leaf(openvarlist(_)).
leaf(openvarlist).
leaf(opencharylist(_,_,_)).
leaf(opencharylist).
leaf(opencodeylist(_,_,_)).
leaf(opencodeylist).
leaf(varlist(_)).
leaf(varlist).
leaf(charylist(_,_,_)).
leaf(charylist).
leaf(codeylist(_,_,_)).
leaf(codeylist).

% =============================================================================

%! texty_settled(@TypeDesc)
%
% Accept valid TypeDesc that describes a "settled" "texty" term, i.e. one 
% which will remain "texty" whatever future instantiations occur (in fact,
% in this case, the TypeDesc describes a ground term). 
%
% texty_settled/1 accepts both the adorned (compound term) form and the
% unadorned form.
%
% Throws assertion error if any of the passed TypeDesc is invalid according to
% texty_typedesc/1 (unless assertions have been disabled).

texty_settled(TypeDesc) :-
   (nonvar(TypeDesc) -> assertion(texty_typedesc(TypeDesc)) ; true),
   settled(TypeDesc).

% =============================================================================

%! texty_settled(@TypeDesc,-Outcome)
%
% Reified vesion of texty_settled/1 which unifies Outcome to either =|settled|=
% or =|unsettled|=.
%
% Example: Give me "leaves" which are "settled":
%
% ```
% ?- texty_leaf(X),texty_settled(X).
% X = number ;
% X = emptylist ;
% X = charlist ;
% X = codelist ;
% X = charlist(_10076) ;
% X = codelist(_10820) ;
% X = atom ;
% X = string ;
% X = atom(_13044) ;
% X = string(_13788) ;
% false.
% ```

texty_settled(TypeDesc,What) :-
   assertion(var(What);What==settled;What==unsettled),
   texty_settled_2(TypeDesc,What).

% The following is designed to permit enumeration:

texty_settled_2(TypeDesc,settled) :- texty_settled_2(TypeDesc).  % fast for enum
texty_settled_2(TypeDesc,unsettled) :- texty_typedesc(TypeDesc),\+texty_settled_2(TypeDesc). % possible to enumerate and test

% ---
% Helper for settled decision. One could also use texty_subsume(anytext,X)
% instead.
% ---

settled(anytext).
settled(number).
settled(emptylist).
settled(text(_)).
settled(text).
settled(stringy(_)).
settled(stringy).
settled(textlist(_)).
settled(textlist).
settled(charlist).
settled(codelist).
settled(charlist(_)).
settled(codelist(_)).
settled(atom).
settled(string).
settled(atom(_)).
settled(string(_)).
