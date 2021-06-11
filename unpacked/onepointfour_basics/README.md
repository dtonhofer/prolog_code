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

## `checks.pl`

A more powerful replacement for `must_be/2`

- [`checks.pl`](checks.pl)
- [`checks.plt`](checks.plt)

`checks.pl` contains a rather large description of what this all about (still not well-written)

The predicates for `checks.pl` are used throughout in other code.

## `stringy_morph.pl`

A "logical" mapper between atoms and SWI-Prolog strings and "list representations"
of character sequences, i.e. proper lists of chars and proper lists of codes.

- [`stringy_morph.pl`](stringy_morph.pl) (MIT license) 
- [`stringy_morph.plt`](stringy_morph.plt) (0BSD license)

 The module provides a replacement for

- [`atom_string/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_string/2),

with a type-information taking/providing predicate `stringy_morph/4` which
restricts the arguments to strings and atoms, and a replacement for all of

- [`atom_codes/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_codes/2) and
- [`string_chars/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=string_chars/2),
- [`string_codes/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=string_codes/2).

with a type-information taking/providing predicate `stringy_charylist_morph/4`, 
restricting arguments to strings and atoms for argument 1, and list of codes or chars on argument 2.

It thus provides a more unified interface than the 4 predicates of SWI-Prolog (which
may be more flexible than their names suggest; for example `atom_string/2` can work
with lists of chars or codes.)

[README_stringy_morph.md](README_stringy_morph.md)

## `space_string.pl` 

Create or accept strings made only of the SPACE character (Unicode code point 0x20).

[README_space_string.md](README_space_string.md)

## `stringy_and_charylist_type.pl`

Simple analysis for stringy and chary terms.

[README_stringy_and_charylist_type.md](README_stringy_and_charylist_type.md)
