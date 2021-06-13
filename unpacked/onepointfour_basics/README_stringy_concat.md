# `stringy_concat.pl`

- [`stringy_concat.pl`](stringy_concat.pl)
- [`stringy_concat.plt`](stringy_concat.plt)

Thois module provides a simple predicate to concatenate a list of _stringys_
into a single _stringy_, which can be requested to be either an atom or a string.

The predicate does not perform splitting.

It is basically a frontend to similar SWI-Prolog predicates but accepts only atoms or strings, not atomics in general.

## Note

SWI-Prolog provides too many predicates dealing with atom/string concatenation

Two concatenable terms as input (can split)

- [atom_concat/3](https://eu.swi-prolog.org/pldoc/man?predicate=atom_concat/3) (ISO)
- [string_concat/3](https://eu.swi-prolog.org/pldoc/man?predicate=string_concat/3)

Two more general concatenable terms as input (cannot split because arguments 1 and 2 are too general)

- [atomic_concat/3](https://eu.swi-prolog.org/pldoc/man?predicate=atomic_concat/3)

A list of concatenable terms as input (never split)

- [atomic_list_concat/2](https://eu.swi-prolog.org/pldoc/man?predicate=atomic_list_concat/2) - generates atom at argument 2. Refuses string at argument 2 in accept mode (that's likely a bug).
- [atomics_to_string/2](https://eu.swi-prolog.org/pldoc/man?predicate=atomics_to_string/2) - generates string at argument 2. Refuses atom at argument 2 in accept mode (that's likely a bug).

A list of concatenable terms as input, and intersperse another string (can split at interspersed string)

- [atomic_list_concat/3](https://eu.swi-prolog.org/pldoc/man?predicate=atomic_list_concat/3) - concatenate with separator ("intersperse", "join"). Refuses string at argument 3 in accept mode (that's likely a bug).
- [atomics_to_string/3](https://eu.swi-prolog.org/pldoc/man?predicate=atomics_to_string/3) - concatenate with separator ("intersperse", "join"). Refuses atom at argument 3 in accept mode (that's likely a bug).

## Examples

```
?- stringy_concat([foo,"bar",baz,"quux"],R,string).
R = "foobarbazquux".

?- stringy_concat([foo,"bar",baz,"quux"],R,atom).
R = foobarbazquux.
```

## TODO

- If all the elements of the input list are of the same type, the output type should be optional and be forced to the elements' input type.
- This predicate loses information; should it generate a list of "length of substrings" when concatenating to make it reversible (one would also have to retain whether the input was string or atom)

