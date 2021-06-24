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
- `stringy_length_type(+Stringy,?Length,?Type)`
- `stringy_length_type(+Stringy,?Length,?Type,@Tuned)`

All predicates throw if:

- `Stringy` is unbound;
- `Stringy` is bound but not a stringy (i.e. not an atom or string);
- `Length` is not an integer if it is bound.

The additional input-only-argument `Tuned` can be set to either `hard` or `soft` to 
control behaviour for negative `Length` and bad `Type`.

If `Tuned` = `hard` the predicates throw instead of failing if:

   - In case `Length` is bound and < 0.
   - In case `Type` is bound and not one of `atom` or `string`.

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

```
?- stringy_length("Hello",-1,hard).
ERROR: check failed : 'domain' error (the culprit is outside the required domain)
ERROR:    message   : the value should fulfill "integer that is >= 0"-ness
ERROR:    culprit   : -1
```

Additionally finding the type:

```
?- stringy_length_type(hello,Length,Type).
Length = 5,
Type = atom.
```

or accepting the type:

```
?- stringy_length_type(hello,Length,atom).
Length = 5.
```

or not as the case may be:

```
?- stringy_length_type(hello,Length,string).
false.
```

Using `hard` mode is always recommended:

```
?- stringy_length_type(hello,Length,gark,hard).
ERROR: check failed : 'domain' error (the culprit is outside the required domain)
ERROR:    message   : the value should fulfill "stringy_typeid"-ness
ERROR:    culprit   : gark
```
