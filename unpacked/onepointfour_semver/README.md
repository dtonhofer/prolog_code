# Semantic versioning string assembler/disassembler/verifier in Prolog

Based on [Semver Spec V2.0.0](https://semver.org/spec/v2.0.0.html)

This code runs in SWI-Prolog 8.3 without using SWI-Prolog special features 
except for the fact that SWI-Prolog prefers "lists of codes" when doing 
Definite Clause Grammar processing, as opposed to "lists of chars".
Adaptations to your favorite Prolog may be needed for this.

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

## License information

- Author: David Tonhofer (ronerycoder@gluino.name) 
- License: [Zero-Clause BSD / Free Public License 1.0.0 (0BSD)](https://opensource.org/licenses/0BSD)

