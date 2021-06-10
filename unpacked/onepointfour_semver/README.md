# Semantic versioning string assembler/disassembler/verifier in Prolog

Based on [Semver Spec V2.0.0](https://semver.org/spec/v2.0.0.html)

This code runs in SWI-Prolog 8.3 without using SWI-Prolog special features.
Note however that SWI-Prolog prefers "lists of codes" (integer Unicode code
points) when doing _Definite Clause Grammar_ processing, as opposed to
"lists of chars". Adaptations to your favorite Prolog may be needed.

There is a `.plt` file containing Prolog unit tests written with the
[plunit](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27))
framework. Note that github can't syntax-highlight the `.plt` file, as
it consider it to be gnuplot code. 

Unit tests also include tests for PCRE (Perl Compatible Regular Expressions) based 
disassembly (based on [`library(pcre)`](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pcre.html%27))). 
These tests not essential and can be commented out if one does not want 
to bother with PCRE.

## Synopsis

### Assemble

```
?- semantic_version(Text,1,2,3,[],[]).
Text = '1.2.3'.
```

### Disassemble

```
?- semantic_version('1.2.3-00x67+alpha.foo-bar.0099',Major,Minor,Path,PreReleaseIds,BuildIds).
Major = 1,
Minor = 2,
Path = 3,
PreReleaseIds = ['00x67'],
BuildIds = [alpha,'foo-bar','0099'] ;  % maybe more solutions
false.                                 % actually no
```

### Verify

```
?- semantic_version('1.2.3-00x67',1,2,3,['00x67'],[]).
true ;  % one proof found, maybe more
false.  % actually no
```

## How to load it

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

