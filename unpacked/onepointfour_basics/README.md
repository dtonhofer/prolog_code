# Some basic predicates that seem useful

## General structure

This directory contains several SWI-Prolog module files (ending in .pl) and their associate unit test files (ending in.plt), which contain 
one or more plunit modules.

The module files are relatively "small" and provide specific functionality. We avoid large modules where in-module interrelationships
are hard to survey.

## Loading a module and running its tests (in SWI-Prolog)

Put the directory just above this package directory 
onto the list of library directories. This can be done with the 
following directive (or command), where you shall replace `/foo/bar` with
the correct path for your machine:

```
:- assertz(file_search_path(library,'/foo/bar/prolog_code/unpacked')).
```

After that:

- Load the module by relative filename, with [`use_module/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=use_module/1).
- Load the accompanying `.plt` file with [`load_test_files/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=load_test_files/1).
- Run the detected plunit tests with [`run_tests/0`](https://eu.swi-prolog.org/pldoc/doc_for?object=run_tests/0).

For example, for the module defined in file `space_string.pl` (note that SWI-Prolog
loads module files _by filename_ not by module name, although the module name is important):

```
?- use_module(library('onepointfour_basics/space_string.pl')).
?- load_test_files([]).
?- run_tests.
```

## Module naming

The name of a module is relatively arbitrary. We always use the atom `onepointfour_basis_X` where `X` is the suffix-less name
of the file in which the module is defined.

## License information

- Author: David Tonhofer (ronerycoder@gluino.name) 
- License for the .pl files: [MIT License](https://opensource.org/licenses/MIT)
- License for the .plt files: [Zero-Clause BSD (0BSD)](https://opensource.org/licenses/0BSD)

# `checks.pl`: A replacement for `must_be/2`

The files

- [`checks.pl`](checks.pl)
- [`checks.plt`](checks.plt)

provide a replacement for the "precondition checks" performed by [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2),
as the latter feel relatively inflexible.

`checks.pl` contains a rather large description of what this all about.

The predicates for `checks.pl` are used throughout in other code.

(Much more info to follow or should be generated via pldoc from the .pl file and put on a web server).

# `space_string.pl`: Create or accept strings made only of SPACE

The files

- [`space_string.pl`](space_string.pl)
- [`space_string.plt`](space_string.plt)

provide predicates to (quickly) create or accept strings consisting
only of the SPACE character (ASCII code point 0x20). 

This is a simple but still interesting exercise. The code uses the
functionality provided by `check.pl` to verify passed arguments and
fail or throw as required.

## Synopsis

```
space_string(?N,?String)           % mostly fails on bad input
space_string(?N,?String,@Throw)    % with Throw=throw or Throw=true, prefers to throw on bad input
space_string_smooth(?N,?Stringy)   % only fails on bad input
```

## Examples

```
?- space_string(10,String).
String = "          ".

?- space_string(N,"    ").
N = 4.

?- space_string(N," hey  ").
false.

?- space_string(-1,S).
false.

?- space_string(-1,S,throw).
ERROR: check failed : domain error (the culprit is outside the required domain)
```

# `stringy_and_charylist_type.pl`: Various operations on _stringy_ and _chary_ terms

New vocabulary:

- A _stringy_ term is a term that is either an _atom_ or a _string_. In SWI-Prolog, the _string_
  is a distinct representation of a sequence of characters, distinct from the _atom_ and
  mean to be used in text processing rather than as basis for identifiers.
- A _chary_ term is a term that is either a _char_ (an atom of length 1) or a _code_ (an integer
  and, more precisely in SWI-Prolog, a Unicode code point).
- A _charylist_ is less precise: it is a proper list of either codes or chars. It may or may not contain 
  uninstantiated elements. An empty list is a _charylist_ but we cannot know whether it is supposed
  to be composed of codes or chars. A list containing only uninstantiated variables is also a _charylist_
  and again we don't know what it is supposed to contain, at least not yet.
    
## Synopsis

```
charylist_type(@CharyList,?Type)
stringy_type(@Stringy,?Type)
stringy_type(@Stringy,?Type,@Throw)
stringy_type_with_length(@Stringy,Type)
stringy_type_with_length(Stringy,Type,Throw)
stringy_length(@Stringy,?Length)
stringy_length(@Stringy,?Length,@Throw)
```
  


