# Some basic predicates that seem useful

## `checks.pl`: A replacement for `must_be/2`

The files

- [`checks.pl`](checks.pl)
- [`checks.plt`](checks.plt)

provide a replacement for the "precondition checks" performed by [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2),
as the latter feel relatively inflexible.

`checks.pl` contains a rather large description of what this all about.

The predicates for `checks.pl` are used throughout in other code.

## `space_string.pl`: Create or accept strings made only of SPACEs

The files

- [`space_string.pl`](space_string.pl)
- [`space_string.plt`](space_string.plt)

provide predicates to (quickly) create or accept strings consisting
only of the SPACE character (ASCII code point 0x20). 

This is a simple but still interesting exercise. The code uses the
functionality provided by `check.pl` to verify passed arguments and
fail or throw as required.

### Synopsis

```
space_string(?N,?String)

space_string(?N,?String,@Throw)

space_string_smooth(?N,?Stringy)
```

### Examples

```
?- space_string(10,String).
String = "          ".

?- space_string(N,"    ").
N = 4.

?- space_string(N," hey  ").
false.
```

### Loading the module and running the tests in SWI-Prolog

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

```
?- use_module(library('onepointfour_basics/space_string.pl')).
?- load_test_files([]).
?- run_tests.
```

## License information

- Author: David Tonhofer (ronerycoder@gluino.name) 
- Licenses: [MIT License](https://opensource.org/licenses/MIT)



