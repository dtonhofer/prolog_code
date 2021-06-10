:- module(onepointfour_semver,
          [
          semantic_version/6  % semantic_version(SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds)
          ]).

% =============================================================================
% Semantic versioning string V2.0.0 assembler/disassembler/verifier in Prolog
%
% Based on https://semver.org/spec/v2.0.0.html
% =============================================================================
% This code runs in SWI-Prolog 8.3 without using SWI-Prolog special features 
% except for the fact that SWI-Prolog prefers "lists of codes" when doing 
% Definite Clause Grammar processing, as opposed to "lists of chars".
% Adaptations to your favorite Prolog may be needed for this.
% =============================================================================
% License information
%
% Author:  David Tonhofer (ronerycoder@gluino.name) 
% License: Zero-Clause BSD / Free Public License 1.0.0 (0BSD)
%          https://opensource.org/licenses/0BSD
% =============================================================================

% -----------------------------------------------------------------------------
% semantic_version(?SemverAtom:atom,
%                  ?Major:int,
%                  ?Minor:Int,
%                  ?Patch:Int,
%                  ?PreReleaseIds:list(atom),
%                  ?BuildIds:list(atom)).
%
% ?SemverAtom:atom     : the semantic version text as an atom
% ?Major:int           : major number as integer >= 0
% ?Minor:Int           : major number as integer >= 0
% ?Patch:Int           : minor number as integer >= 0
% ?PreReleaseIds:list  : possibly empty list of atoms and integers. If
%                        the element contains only digits, it is
%                        interpreted as an integer rather than "text"
%                        and in that case leading zeros are not allowed.
%                        Allowed text chars are letter A-Za-z, digits 0-9
%                        and the dash.
% ?BuildIds:list       : possibly empty list of atoms (if the element
%                        contains only digits, it is still interpreted
%                        as "text" and may have leading zeros)
%                        Allowed text chars are letter A-Za-z, digits 0-9
%                        and the dash.
% -----------------------------------------------------------------------------

semantic_version(SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds) :-
   (nonvar(SemverAtom) % the instantiation state of SemverAtom is key here
    -> disassemble(SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds)
    ;  assemble(SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds)).

% ---

% assemble/6 performs various domain checks on the individual arguments.
% It throws if the values are unexpected.

assemble(SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds) :-
   entry_test_assemble(Major,Minor,Patch,PreReleaseIds,BuildIds,BuildIds2),
   number_textlist(Major,MajorCs),
   number_textlist(Minor,MinorCs),
   number_textlist(Patch,PatchCs),
   cookuncook_prerelease_ids(UncookedPreReleaseIds,PreReleaseIds), % uncooked = list of tagged list of code/char
   cookuncook_build_ids(UncookedBuildIds,BuildIds2),               % uncooked = list of code/char
   % Call the DCG grammar; this will leave a sequence of codes in Cs,
   % which then will be fused into an atom.
   once(phrase(semver(MajorCs,MinorCs,PatchCs,UncookedPreReleaseIds,UncookedBuildIds),Cs,[])),
   atom_textlist(SemverAtom,Cs).

% ---

% A quick check of passed arguments.
% Deeper checks are actually done via the grammar.

entry_test_assemble(Major,Minor,Patch,PreReleaseIds,BuildIdsIn,BuildIdsOut) :-
   must_be(nonneg,Major), % will throw if not "nonneg integer"
   must_be(nonneg,Minor), % will throw if not "nonneg integer"
   must_be(nonneg,Patch), % will throw if not "nonneg integer"
   verify_prerelease_ids(PreReleaseIds), % will throw if bad content
   verify_transform_build_ids(BuildIdsIn,BuildIdsOut). % will throw if bad content; for convenience, transform integers to atoms

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

verify_transform_build_ids([A|Xs],[A|Os]) :-
   atom(A), % correct form of atom must be "alphanumeric" or "all digits", this will be checked by the grammar
   !,
   verify_transform_build_ids(Xs,Os).

verify_transform_build_ids([N|Xs],[A|Os]) :-
   number(N),
   !,
   atom_number(A,N), % for convenience, transform integers to atoms
   verify_transform_build_ids(Xs,Os).

verify_transform_build_ids([],[]) :- !.

verify_transform_build_ids([X|_],[_|_]) :-
   type_error("build id id must be atom or possibly a number",X).

% ---

% disassemble/6 fails if the atom "SemverAtom" cannot be properly parsed.
% Unfortunately, this won't tell us what's wrong with it. Otherwise
% this is very straightforward. The implementation has no problem in case
% some of the arguments "Major","Minor","Patch","PreReleaseIds","BuildIds" are
% already instantiated at call time. It will fail if unification with these
% fails.

disassemble(SemverAtom,Major,Minor,Patch,PreReleaseIds,BuildIds) :-
   atom_textlist(SemverAtom,Cs),
   phrase(semver(MajorCs,MinorCs,PatchCs,UncookedPreReleaseIds,UncookedBuildIds),Cs,[]),
   number_textlist(Major,MajorCs),
   number_textlist(Minor,MinorCs),
   number_textlist(Patch,PatchCs),
   cookuncook_prerelease_ids(UncookedPreReleaseIds,PreReleaseIds),
   cookuncook_build_ids(UncookedBuildIds,BuildIds).

% ---

cookuncook_build_ids([UC|UCs],[CC|CCs]) :-
   atom_textlist(CC,UC),
   cookuncook_build_ids(UCs,CCs).

cookuncook_build_ids([],[]).

% ---

cookuncook_prerelease_ids([alphanumeric(UC)|UCs],[CC|CCs]) :-
   (atom(CC);var(CC)),
   atom_textlist(CC,UC),
   cookuncook_prerelease_ids(UCs,CCs).

cookuncook_prerelease_ids([numeric(UC)|UCs],[CC|CCs]) :-
   ((integer(CC),CC>=0);var(CC)),
   number_textlist(CC,UC),
   cookuncook_prerelease_ids(UCs,CCs).

cookuncook_prerelease_ids([],[]).

% ---

% Transform between an "atom" and a "list of codes".
% Works for directions Atom->Cs, Cs->Atom and accepts (Atom,Cs)

atom_textlist(Atom,Cs) :-
   atom_codes(Atom,Cs).

% ---

% Transform between a "number" and a "list of codes"
% Works for directions Atom->Cs, Cs->Atom and accepts (Atom,Cs)

number_textlist(Number,Cs) :-
   number_codes(Number,Cs).

% =============================================================================
% Definite Clause Grammer for semantic versioning string.
% This is based on processing of lists of "codes" (rather than "chars"), i.e.
% the original characters are mapped to their Unicode codepoints (integers)
% before processing by the DCG. This is SWI-Prolog's preferred style. In
% particular, in SWI-Prolog, any text enclosed in double quotes ("hello")
% at the level of the DCG is exploded into a list of codes by default.
% Other Prolog may prefer "chars" (atoms of length 1) or even have a dedicated
% character type. Some adaptations may be needed.
% =============================================================================

% For semver//5, attempt "most often encountered" match first. We can always
% try again if this fails.
% - "Maj", "Min", "Pat" are nonempty lists of codes representing integers.
% - "Pre" is a nonempty list of nonempty lists of codes representing
%   alphanumeric strings
% - "Bld" is a nonempty list of tagged nonempty lists of codes representing
%   alphanumeric strings (tagged as "alphanumeric(List)") or integers
%   (tagged as numeric(List)").
% To have call uniformity, all the semver//5 DCG rules take the same arguments.

semver(Maj,Min,Pat,[],[]) -->
   version_core(Maj,Min,Pat).

semver(Maj,Min,Pat,[],Bld) -->
   version_core(Maj,Min,Pat),
   plus,
   build_ids(Bld).

semver(Maj,Min,Pat,Pre,[]) -->
   version_core(Maj,Min,Pat),
   dash(_),
   prerelease_ids(Pre).

semver(Maj,Min,Pat,Pre,Bld) -->
   version_core(Maj,Min,Pat),
   dash(_),
   prerelease_ids(Pre),
   plus,
   build_ids(Bld).

version_core(Maj,Min,Pat) -->
   major(Maj),
   dot,
   minor(Min),
   dot,
   patch(Pat).

major(Cs) -->  numeric_id(Cs).
minor(Cs) -->  numeric_id(Cs).
patch(Cs) -->  numeric_id(Cs).

% Non-greedy ordering for pre-release//1: Try a short match first.
% We can always backtrack later.

prerelease_ids([Id])     --> prerelase_id(Id).
prerelease_ids([Id|Ids]) --> prerelase_id(Id), dot, prerelease_ids(Ids).

% Non-greedy ordering for build//1: Try short match first.
% We can always backtrack later.

build_ids([Id])     --> build_id(Id).
build_ids([Id|Ids]) --> build_id(Id), dot, build_ids(Ids).

% The list of code underlying "pre-release ids" are "tagged":
% - The term "alphanumeric(Cs)" is used for a list of codes that represents
%   an alphanumeric string;
% - The term "numeric(Cs)" is used for a list of char/codes that represents
%   an integer.

prerelase_id(alphanumeric(Cs)) --> alphanum_id(Cs).
prerelase_id(numeric(Cs))      --> numeric_id(Cs).

build_id(Cs) --> alphanum_id(Cs).
build_id(Cs) --> any_digits(Cs).

% A helper predicate for alphanum_id//1

splinter(Cs,Prefix,C,Suffix) :- append(Prefix,[C|Suffix],Cs).

% This rule allows us to avoid copying rules of alphanum_id//1
% It introduces EBNF! Note the non-greedy ordering.

optional_id_chars([]) --> [].
optional_id_chars(Cs) --> id_chars(Cs).

% Non-greedy ordering for alphanum_id//1: Try short match first.
% We can always backtrack later.

alphanum_id([C|Cs]) --> non_digit(C), optional_id_chars(Cs).

% ===========

% The definition based on the original BNF formulation leads to a
% much non-determinism, which is why it is commented out and
% the second one is used.

/*
alphanum_id(Cs) -->
   {var(Cs)},
   id_chars(Prefix),
   non_digit(C),
   optional_id_chars(Suffix),
   {splinter(Cs,Prefix,C,Suffix)}. % Use splinter/4 to merge the parts

% This one is used when we have codes in Cs that we verify against
% actual codes.

alphanum_id(Cs) -->
   {nonvar(Cs)},
   {splinter(Cs,Prefix,C,Suffix)}, % Use splinter/4 to propose parts
   id_chars(Prefix),
   non_digit(C),
   optional_id_chars(Suffix).
*/

% ==========

alphanum_id(Cs) -->
   {var(Cs)},
   any_digits(Prefix),
   non_digit(C),
   optional_id_chars(Suffix),
   {splinter(Cs,Prefix,C,Suffix)}. % Use splinter/4 to merge the parts

alphanum_id(Cs) -->
   {nonvar(Cs)},
   {splinter(Cs,Prefix,C,Suffix)}, % Use splinter/4 to propose parts
   any_digits(Prefix),
   non_digit(C),
   optional_id_chars(Suffix).

% ==========

numeric_id([C])    --> zero(C).
numeric_id([C])    --> non_zero_digit(C).
numeric_id([C|Cs]) --> non_zero_digit(C), any_digits(Cs).

% digit//2 has been renamed to any_digit//2 and
% digits//2 has been renamed to any_digits//1 to avoid
% clashes with existing digit//1 and digits//1 from the libraries

any_digit(C)       --> zero(C).
any_digit(C)       --> non_zero_digit(C).
any_digits([C])    --> any_digit(C).
any_digits([C|Cs]) --> any_digit(C), any_digits(Cs).

id_chars([C])      --> id_char(C).
id_chars([C|Cs])   --> id_char(C), id_chars(Cs).

non_digit(C)       --> letter(C).
non_digit(C)       --> dash(C).

id_char(C)         --> any_digit(C).
id_char(C)         --> letter(C).
id_char(C)         --> dash(C).

% We are reaching the lowest level: codes in the input list.
% Tests for code ranges are done explicitly using between/3.
% Luckily, we know that the codes are Unicode codes and not
% EBCDIC or something equally bizarre.

dash(0'-) --> "-". % Code representation of "-" in the rule head
zero(0'0) --> "0". % Code representation of "0" in the rule head
plus      --> "+".
dot       --> ".".

non_zero_digit(C) --> [C],{between(0'1,0'9,C)}.
letter(C)         --> [C],{between(0'A,0'Z,C);between(0'a,0'z,C)}.

