# Semantic versioning string assembler/disassembler/verifier in Prolog

The present code offers a "semantic versioning string" assembler/disassembler/verifier in Prolog.
It handles strings that follow the [Semver V2.0.0](https://semver.org/spec/v2.0.0.html) specification.

The code runs in SWI-Prolog 8.3 without using SWI-Prolog special features.

Note that SWI-Prolog prefers "lists of codes" (integer Unicode code
points) when doing _Definite Clause Grammar_ processing, as opposed to
"lists of chars". Adaptations to your favorite Prolog may be needed.

There are two files:

- [`semver.pl`](semver.pl) - Code in the form of an [SWI-Prolog module](https://eu.swi-prolog.org/pldoc/man?section=modules).
- [`semver.plt`](semver.plt) - Unit tests in the form of several [plunit](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) modules. (Note that github can't syntax-highlight the `.plt` file correctly. It consider it to be gnuplot code, leading to a mess.)

The `.plt` file includes unit tests for semver string disassembly 
based on PCRE (Perl Compatible Regular Expressions) as provided by
[`library(pcre)`](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pcre.html%27))). 
These tests not essential and can be commented out if one does not want to bother with PCRE.

## Synopsis

```
semantic_version(?SemverAtom:atom,
                 ?Major:integer,
                 ?Minor:integer,
                 ?Patch:integer,
                 ?PreReleaseIds:list(atom),
                 ?BuildIds:list(atom)).
```

## Examples

### Assemble semver string from components

```
?- semantic_version(Text,1,2,3,[],[]).
Text = '1.2.3'.
```

### Disassemble semver string into its components

```
?- semantic_version('1.2.3-00x67+alpha.foo-bar.0099',Major,Minor,Path,PreReleaseIds,BuildIds).
Major = 1,
Minor = 2,
Path = 3,
PreReleaseIds = ['00x67'],
BuildIds = [alpha,'foo-bar','0099'] ;  % maybe more solutions
false.                                 % actually no
```

### Verify that a string corresponds to components

```
?- semantic_version('1.2.3-00x67',1,2,3,['00x67'],[]).
true ;  % one proof found, maybe more
false.  % actually no
```

## How to load the module

If you have the directory just above this package directory on you list
of library directories, which can be achieved with

```
:- assertz(file_search_path(library,'/foo/bar/prolog_code/unpacked')).
```

then you can load this module with

```
:- use_module(library('onepointfour_semver/semver.pl')).
```

and run the tests with:

```
:- load_test_files([]).
:- run_tests.
```

## See also

[Announcement on the semver issue tracker on 2021-Feb-24: Issue 667](https://github.com/semver/semver/issues/667)

## License information

- Author: David Tonhofer (ronerycoder@gluino.name) 
- License: [Zero-Clause BSD / Free Public License 1.0.0 (0BSD)](https://opensource.org/licenses/0BSD)

