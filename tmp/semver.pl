:- module(semver, 
          [
          semantic_version/7  % semantic_version(How,SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds)
          ]).
          
% =============================================================================
% Semantic versioning assembler/disassembler V 1.0.0
%
% Based on https://semver.org/spec/v2.0.0.html
% =============================================================================
% Runs in SWI-Prolog 8.3 without using SWI-Prolog special features.
%
% As Prolog is not fully standardized, this may still need adaptations for
% other Prologs.
%
% However:
%
% - The Unit Test code is based on the "plunit" framework available
%   in SICStus and SWI-Prolog and maybe other
%   (see https://eu.swi-prolog.org/pldoc/man?section=porting)
%
% This code is "overkill in flexibility" as it accomodates the parsing of
% an input atom that is decomposed into a "lists of characters" or
% a "lists of codes". Depending on preferences and the "natural way" of
% handling this in your Prolog, you may want to simplify.
%
% Author:  David Tonhofer ronerycoder@gluino.name
% License: https://opensource.org/licenses/0BSD 
%          "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
%
% History: 2021-02-24 - 1.0.0
% =============================================================================

% plunit tests

:- begin_tests(semver).

test("empty is not allowed",fail) :-
   semantic_version(chars,'',_,_,_,_,_).

test("major-minor-patch disassemble 1",[nondet,true([Major,Minor,Patch,X,Y]==[1,2,3,[],[]])]) :-
  semantic_version(chars,'1.2.3',Major,Minor,Patch,X,Y).

test("major-minor-patch disassemble 2",[nondet,true([Major,Minor,Patch,X,Y]==[0,0,0,[],[]])]) :-
  semantic_version(chars,'0.0.0',Major,Minor,Patch,X,Y).

test("major-minor-patch disassemble 3",[nondet,true([Major,Minor,Patch,X,Y]==[3,2,1,[],[]])]) :-
  semantic_version(chars,'3.2.1',Major,Minor,Patch,X,Y).
  
test("major-minor-patch disassemble failure: no leading zeros",fail) :-
  semantic_version(chars,'03.2.1',_,_,_,_,_).

test("major-minor-patch disassemble failure: no alphanumerics",fail) :-
  semantic_version(chars,'v3.2.1',_,_,_,_,_).

test("major-minor-patch assemble 1",[true(Text=='1.2.3')]) :-
  semantic_version(chars,Text,1,2,3,[],[]).

test("major-minor-patch assemble 2",[true(Text=='0.0.0')]) :-
  semantic_version(chars,Text,0,0,0,[],[]).
  
test("bad pre-release id #1",fail) :-
   semantic_version(chars,_,1,2,3,['0067'],[]). % must be integer or alphanumeric; fails because the grammar fails

test("bad pre-release id #2",[error(type_error(_,_))]) :-
   semantic_version(chars,_,1,2,3,[3.1415],[]). % must be integer or alphanumeric; throws because entry test catches it
   
test("disassemble+assemble #1",[nondet,true(Text=='1.2.3')]) :-
  semantic_version(chars,Text,1,2,3,[],[]),
  semantic_version(chars,Text,1,2,3,[],[]).

test("disassemble+assemble #2",[nondet,true(Text== '1.2.3-10.20.30.40+foo.bar.baz')]) :-
  semantic_version(chars,Text,1,2,3,[10,20,30,40],[foo,bar,baz]),
  semantic_version(chars,Text,1,2,3,[10,20,30,40],[foo,bar,baz]).

test("disassemble+assemble #3",[nondet,true(Text=='1.2.3-10.abc.4-5.00x67.order66')]) :-
  semantic_version(chars,Text,1,2,3,[10,abc,'4-5','00x67',order66],[]),
  semantic_version(chars,Text,1,2,3,[10,abc,'4-5','00x67',order66],[]).

test("disassemble+assemble #4",[nondet,true(Text== '1.2.3-00x67+alpha.foo-bar.0099')]) :-
  semantic_version(chars,Text,1,2,3,['00x67'],[alpha,'foo-bar','0099']),
  semantic_version(chars,Text,1,2,3,['00x67'],[alpha,'foo-bar','0099']).

test("disassemble+assemble #5",[nondet,true(Text== '1.2.3+bravo.foo-bar.0099')]) :-
  semantic_version(chars,Text,1,2,3,[],[bravo,'foo-bar','0099']),
  semantic_version(chars,Text,1,2,3,[],[bravo,'foo-bar','0099']).

:- end_tests(semver).

% -----------------------------------------------------------------------------
% semantic_version(+How:atom,
%                  ?SemverAtom:atom,
%                  ?Major:int,
%                  ?Minor:Int,
%                  ?Patch:Int,
%                  ?PreReleaseIds:list(atom),
%                  ?BuildIds:list(atom)).
% ---
% +How:atom,            - how to do processing; one of 'char' or 'codes'
% ?SemverAtom:atom,     - the semantic version text as an atom
% ?Major:int,           - major number as integer >= 0
% ?Minor:Int,           - major number as integer >= 0
% ?Patch:Int,           - minor number as integer >= 0
% ?PreReleaseIds:list,  - possibly empty list of atoms and integers. If
%                         the element contains only digits, it is
%                         interpreted as an integer rather than "text"
%                         and in that case leading zeros are not allowed.
%                         Allowed text chars are letter A-Za-z, digits 0-9
%                         and the dash.
% ?BuildIds:list.       - possibly empty list of atoms (if the element
%                         contains only digits, it is still interpreted
%                         as "text" and may have leading zeros)
%                         Allowed text chars are letter A-Za-z, digits 0-9
%                         and the dash.
% -----------------------------------------------------------------------------

semantic_version(How,SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds) :-
   must_be(oneof([chars,codes]),How),
   (nonvar(SemverAtom) % the instantiation state of SemverAtom is key here
    -> disassemble(How,SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds)
    ; assemble(How,SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds)).  
    
% --- 
% assemble/7 performs various domain checks on the individual arguments
% and throws if the values are unexpected.
% --- 

assemble(How,SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds) :-
   entry_test_assemble(Major,Minor,Patch,PreReleaseIds,BuildIds,BuildIds2),  
   number_textlist(How,Major,MajorCs),
   number_textlist(How,Minor,MinorCs),
   number_textlist(How,Patch,PatchCs),
   cookuncook_prerelease_ids(How,UncookedPreReleaseIds,PreReleaseIds), % uncooked = list of tagged list of code/char
   cookuncook_build_ids(How,UncookedBuildIds,BuildIds2), % uncooked = list of code/char
   % Call the DCG grammar; this will leave a sequence of characters in Cs, which then just has to be fused into an atom
   once(phrase(semver(How,MajorCs,MinorCs,PatchCs,UncookedPreReleaseIds,UncookedBuildIds),Cs,[])),
   atom_textlist(How,SemverAtom,Cs).

% ---
% A quick check of passed arguments; deeper checks are actually done via the grammar.
% ---

entry_test_assemble(Major,Minor,Patch,PreReleaseIds,BuildIdsIn,BuildIdsOut) :-
   must_be(nonneg,Major), % will throw if not "nonneg integer"
   must_be(nonneg,Minor), % will throw if not "nonneg integer"
   must_be(nonneg,Patch), % will throw if not "nonneg integer"
   verify_prerelease_ids(PreReleaseIds), % will throw if bad content
   verify_build_ids(BuildIdsIn,BuildIdsOut). % will throw if bad content; for convenience, transform integers to atoms

% ---

verify_prerelease_ids([X|Xs]) :-
   integer(X),X>=0,
   !,
   verify_prerelease_ids(Xs).
   
verify_prerelease_ids([X|Xs]) :-
   atom(X), % correct form of atom must be "alphanumeric", this will be checked by the grammar
   !,
   verify_prerelease_ids(Xs).
   
verify_prerelease_ids([]) :- !.

verify_prerelease_ids([X|_]) :-
   type_error("pre-release id must be atom or positive-or-zero integer",X).

% ---

verify_build_ids([A|Xs],[A|Os]) :-
   atom(A), % correct form of atom must be "alphanumeric" or "all digits", this will be checked by the grammar
   !,
   verify_build_ids(Xs,Os).

verify_build_ids([N|Xs],[A|Os]) :-
   number(N),
   !,
   atom_number(A,N),
   verify_build_ids(Xs,Os).
   
verify_build_ids([],[]) :- !.

verify_build_ids([X|_],[_|_]) :-
   type_error("build id id must be atom or possibly a number",X).
 
% --- 
% disassemble/7 fails if the atom SemverAtom cannot be properly parsed.
% Unfortunately, this won't tell us what's wrong with it. Otherwise
% this is very straightforward. The implementation has no problem in case
% some of the arguments Major,Minor,Patch,PreReleaseIds,BuildIds are 
% already instantiated at call time. It will fail if unification with these
% fails.
% ---

disassemble(How,SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds) :-
   atom_textlist(How,SemverAtom,Cs),
   phrase(semver(How,MajorCs,MinorCs,PatchCs,UncookedPreReleaseIds,UncookedBuildIds),Cs,[]), 
   number_textlist(How,Major,MajorCs),
   number_textlist(How,Minor,MinorCs),
   number_textlist(How,Patch,PatchCs),
   cookuncook_prerelease_ids(How,UncookedPreReleaseIds,PreReleaseIds),
   cookuncook_build_ids(How,UncookedBuildIds,BuildIds).
   
% ---
   
cookuncook_build_ids(How,[UC|UCs],[CC|CCs]) :-
   atom_textlist(How,CC,UC),
   cookuncook_build_ids(How,UCs,CCs).

cookuncook_build_ids(_,[],[]).
     
% ---

cookuncook_prerelease_ids(How,[alphanumeric(UC)|UCs],[CC|CCs]) :-
   (atom(CC);var(CC)),
   atom_textlist(How,CC,UC),
   cookuncook_prerelease_ids(How,UCs,CCs).

cookuncook_prerelease_ids(How,[numeric(UC)|UCs],[CC|CCs]) :-
   ((integer(CC),CC>=0);var(CC)),
   number_textlist(How,CC,UC),
   cookuncook_prerelease_ids(How,UCs,CCs).
   
cookuncook_prerelease_ids(_,[],[]).
   
% ---
% Transform between an "atom" and a "list of chars"/"list of codes".
% Works for directions Atom->Cs, Cs->Atom and accepts (Atom,Cs)
% ---

atom_textlist(chars,Atom,Cs) :- 
   atom_chars(Atom,Cs).
   
atom_textlist(codes,Atom,Cs) :- 
   atom_codes(Atom,Cs).

% ---
% Transform between a "number" and a "list of chars"/"list of codes"
% Works for directions Atom->Cs, Cs->Atom and accepts (Atom,Cs)
% ---

number_textlist(chars,Number,Cs) :-    
   number_chars(Number,Cs).
   
number_textlist(codes,Number,Cs) :- 
   number_codes(Number,Cs).

% =============================================================================
% Actual DCG, the core of everything. The "How" is always an atom 'chars' or
% 'codes' indicating whether we are working on an input list of chars or
% codes.
% =============================================================================

% Most often encountered match first for semver//2; we can always try again if an empty "Rest" of chars is demanded
% "Maj", "Min", "Pat" are nonempty lists of chars/codes representing integers
% "Pat" is a nonempty list of nonempty lists of chars/codes representing alphanumeric strings
% "Bld" is a nonempty list of tagged nonempty lists of chars/codes representing alphanumeric strings or integers
% To have call uniformity and avoid headaches, all the semver//6 DCG rules take the same arguments.

semver(How,Maj,Min,Pat,[],[]) -->
   version_core(How,Maj,Min,Pat).

semver(How,Maj,Min,Pat,[],Bld) -->
   version_core(How,Maj,Min,Pat),
   plus(How),
   build_ids(How,Bld).

semver(How,Maj,Min,Pat,Pre,[]) -->
   version_core(How,Maj,Min,Pat),
   dash(How,_),
   prerelease_ids(How,Pre).
   
semver(How,Maj,Min,Pat,Pre,Bld) -->
   version_core(How,Maj,Min,Pat),
   dash(How,_),
   prerelease_ids(How,Pre),
   plus(How),
   build_ids(How,Bld).
   
% ---

version_core(How,Maj,Min,Pat) -->
   major(How,Maj),
   dot(How),
   minor(How,Min),
   dot(How),
   patch(How,Pat).

% ---

major(How,Cs) -->
   numeric_id(How,Cs).

% ---

minor(How,Cs) -->
   numeric_id(How,Cs).

% ---

patch(How,Cs) -->
   numeric_id(How,Cs).

% ---

% Non-greedy ordering for pre-release//1. We can always backtrack later.

prerelease_ids(How,[Id]) -->
   prerelase_id(How,Id).

prerelease_ids(How,[Id|Ids]) -->
   prerelase_id(How,Id),
   dot(How),
   prerelease_ids(How,Ids).

% ---

% non-greedy ordering for build//1

build_ids(How,[Id]) -->
   build_id(How,Id).

build_ids(How,[Id|Ids]) -->
   build_id(How,Id),
   dot(How),
   build_ids(How,Ids).

% ---

% The values for "pre-release ids" are tagged:
% alphanumeric(Cs) for a list of char/codes that represents and alphanumeric string
% numeric(Cs)  for a list of char/codes that represents an integer

prerelase_id(How,alphanumeric(Cs)) -->
   alphanum_id(How,Cs).

prerelase_id(How,numeric(Cs)) -->
   numeric_id(How,Cs).

% ---
   
build_id(How,Cs) -->
   alphanum_id(How,Cs).

build_id(How,Cs) -->
   any_digits(How,Cs).

% ---
% non-greedy ordering for alphanum_id//1

% Note that this definition based on the original BNF
% formulation is ambiguous/hard to read, which is why it is commented
% out and the second one is used

/*
alphanum_id(How,[C])    --> non_digit(How,C).
alphanum_id(How,[C|Cs]) --> non_digit(How,C),id_chars(How,Cs).
alphanum_id(How,Id)     --> id_chars(How,Cs),non_digit(How,C),{append(Cs,[C],Id)}.
alphanum_id(How,Id)     --> id_chars(How,Cs),non_digit(How,C),id_chars(How,CsTail),{append(Cs,[C|CsTail],Id)}.
*/

alphanum_id(How,[C]) -->
   non_digit(How,C).

alphanum_id(How,[C|Cs]) -->
   non_digit(How,C),
   id_chars(How,Cs).

alphanum_id(How,Cs) -->
   {var(Cs)},
   any_digits(How,Cs1),
   non_digit(How,C),
   id_chars(How,Cs2),
   {append(Cs1,[C|Cs2],Cs)}.

alphanum_id(How,Cs) -->
   {nonvar(Cs)},
   {append(Cs1,[C|Cs2],Cs)},
   any_digits(How,Cs1),
   non_digit(How,C),
   id_chars(How,Cs2).
      
% ---

numeric_id(How,[C]) -->
   zero(How,C).

numeric_id(How,[C]) -->
   nonzero(How,C).

numeric_id(How,[C|Cs]) -->
   nonzero(How,C),
   any_digits(How,Cs).

% ---

% digit//2 has been renamed to any_digit//2 and
% digits//2 has been renamed to any_digits//1 to avoid
% clashes with existing digit//1 and digits//1 from the libraries

any_digit(How,C) -->
   zero(How,C).

any_digit(How,C) -->
   nonzero(How,C).

any_digits(How,[C]) -->
   any_digit(How,C).

any_digits(How,[C|Cs]) -->
   any_digit(How,C),
   any_digits(How,Cs).

% ---

id_chars(How,[C]) -->
   id_char(How,C).

id_chars(How,[C|Cs]) -->
   id_char(How,C),
   id_chars(How,Cs).

% ---

non_digit(How,C) -->
   letter(How,C).

% ---

non_digit(How,C) -->
   dash(How,C).

% ---   

id_char(How,C) -->
   any_digit(How,C).
   
id_char(How,C) --> 
   letter(How,C).
   
id_char(How,C) --> 
   dash(How,C).

% ---   

% The coding trick of having the 'C' on the left side too allows to grab the
% representation as char or code and pass it upwards through the parsing tree.

dash(chars,'-') --> ['-'].
dash(codes,0'-) --> [0'-].

zero(chars,'0') --> ['0'].
zero(codes,0'x) --> [0'x].

plus(chars) --> ['+'].
plus(codes) --> [0'+].

dot(chars) --> ['.'].
dot(codes) --> [0'.].

% Called these nonzero//1 instead of positive_digit//1 because 0 _is_ a
% positive digit, and digits are positive by their very nature

nonzero(chars,C) --> [C],{member(C,['1','2','3','4','5','6','7','8','9'])}.
nonzero(codes,C) --> [C],{member(C,[0'1,0'2,0'3,0'4,0'5,0'6,0'7,0'8,0'9])}.

letter(chars,C) --> [C],{member(C,['A','B','C','D','E','F','G','H','I','J','K','L','M',
                                   'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
                                   'a','b','c','d','e','f','g','h','i','j','k','l','m',
                                   'n','o','p','q','r','s','t','u','v','w','x','y','z'])}.

letter(codes,C) --> [C],{member(C,[0'A,0'B,0'C,0'D,0'E,0'F,0'G,0'H,0'I,0'J,0'K,0'L,0'M,
                                   0'N,0'O,0'P,0'Q,0'R,0'S,0'T,0'U,0'V,0'W,0'X,0'Y,0'Z,
                                   0'a,0'b,0'c,0'd,0'e,0'f,0'g,0'h,0'i,0'j,0'k,0'l,0'm,
                                   0'n,0'o,0'p,0'q,0'r,0's,0't,0'u,0'v,0'w,0'x,0'y,0'z])}.
