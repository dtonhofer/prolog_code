[Top README](../README.md)

# `stringy_length.pl`

- [`stringy_length.pl`](../stringy_length.pl) (MIT license)
- [`stringy_length.plt`](../stringy_length.plt) (0BSD license)

Determine length of an atom or an SWI-Prolog string (a "stringy thing"). 
This is a mashup of Prolog's ISO-standard 
[`atom_length/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_length/2)
and the additional SWI-Prolog 
[`string_length/2`](https://eu.swi-prolog.org/pldoc/man?predicate=string_length/2).
Because having several `X_length/2` to handle "stringy things" just
doesn't make much sense in an anytyped language. Note that the
_implementation_ of SWI-Prolog's `atom_length/2` and `string_length/2`
accepts atoms and strings equally.

The predicates in this module do not take a third argument to 
positively identify the actual type of the "stringy thing". That
third argument would be an atom and one of `atom` or string`. Much
more interesting. There is a predicate like that in module
"stringy_and_charylist_type.pl". TODO: Add it here!

## Loading the module and running its tests (in SWI-Prolog)

Please refer to the [README.md](../README.md) file, but in short:

```
?- assertz(file_search_path(library,'/foo/bar/prolog_code/unpacked')).
?- use_module(library('onepointfour_basics/stringy_length.pl')).
?- load_test_files([]).
?- run_tests.
```

## Synopsis

- `stringy_length(+Stringy,?Length)`
- `stringy_length(+Stringy,?Length,@Tuned)`

Both predicates throw if:

- `Stringy` is unbound;
- `Stringy` is bound but not a stringy (i.e. not an atom or string);
- `Length` is not an integer if it is bound.

The additional input-only-argument `Tuned` can be set to either `hard` or `soft` to 
control behaviour for negative `Length`. 

In case `Length` < 0:

- `stringy_length/2` _fails_ (behaves "softly")
- `stringy_length/3` _fails_ if `Tuned` = `soft` 
- `stringy_length/3` _throws_ if `Tuned` = `hard` 

## Examples

Determining length of a string:

```
?- stringy_length("Hello",L).
L = 5.
```

Determining length of an atom:

```
?- stringy_length('Hello',5).
true.
```

Verifying length of a string:

```
?- stringy_length("Hello",100).
false.
```

What does it say anout negative length?

```
?- stringy_length("Hello",-1).
false.
```

The above is a reasonably "logical" response, but you may want more discipline:

``
?- stringy_length("Hello",-1,hard).
ERROR: check failed : 'domain' error (the culprit is outside the required domain)
ERROR:    message   : the value should fulfill "integer that is >= 0"-ness
ERROR:    culprit   : -1
```

