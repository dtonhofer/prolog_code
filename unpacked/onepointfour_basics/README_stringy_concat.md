# `stringy_concat.pl`

- [`stringy_concat.pl`](stringy_concat.pl)
- [`stringy_concat.plt`](stringy_concat.plt)

This module provides a simple predicate to relate a list of _stringys_ (a _stringy_ being something
that is either a Prolog _atom_ or an SWI-Proog _string_) to a single stringy and its type.
This latter stringy is the concatenation of the stringys in the list, and its type is one of the
atoms `atom` or `string`. The predicate does not perform splitting.

It is basically a frontend to similar SWI-Prolog predicates that accepts only atoms or strings, not atomics in general,
but for which you can either test or request the type of the concatenation stringy.

## Synposis

- `stringy_concat(ListOfStringy,?Result,?ResultType)`
- `stringy_concat(ListOfStringy,Result,ResultType,Tuned)`
 
## Examples

Demand that concatenated stringy be a string by giving `string` as type:

```
?- stringy_concat([foo,"bar",baz,"quux"],R,string).
R = "foobarbazquux".
```

Demand that concatenated stringy be a string by giving `atom` as type:

```
?- stringy_concat([foo,"bar",baz,"quux"],R,atom).
R = foobarbazquux.
```

Ask whether to accept a concatenated stringy, also obtaining its type:

```
?- stringy_concat([foo,"bar",baz,"quux"],foobarbazquux,Type).
Type = atom.
```

Ask whether to accept a concatenated stringy, but the query fails:

```
?- stringy_concat([foo,"bar",baz,"quux"],shazam,Type).
false.
```

Bad type value leads to failure:

```
?- stringy_concat([foo,"bar",baz,"quux"],R,nonsesuch).
false.
```

That can be changed by passing `hard` as additional `Tuned` parameter. This
can be very useful to hunt for bugs. 

```
?- stringy_concat([foo,"bar",baz,"quux"],R,nonsesuch,hard).
ERROR: check failed : domain error (the culprit is outside the required domain)
ERROR:    message   : the value should fulfill 'stringy_typeid-ness'
ERROR:    culprit   : nonsesuch
```

Switching to `soft` restores the behaviour of the 3-argument predicate:

```
?- stringy_concat([foo,"bar",baz,"quux"],R,nonsesuch,soft).
false.
```

Passing something other than a stringy in the list always throws:

```
?- stringy_concat([foo,1],R,string,soft).
ERROR: check failed : type error (the culprit is not of the required type)
ERROR:    message   : the value should fulfill 'stringy-ness'
ERROR:    culprit   : 1
```

```
?- stringy_concat([foo,1],R,string,hard).
ERROR: check failed : type error (the culprit is not of the required type)
ERROR:    message   : the value should fulfill 'stringy-ness'
ERROR:    culprit   : 1
```

## TODO

- `stringy_concat([a,b,c],L,Type).` disappointingly throws. It should just generate the two possibilities.
- If all the elements of the input list are of the same type, the output type should be optional and be forced to the elements' input type.
- This predicate loses information; should it generate a list of "length of substrings" when concatenating to make it reversible (one would also have to retain whether the input was string or atom)

## Note

SWI-Prolog provides too many predicates dealing with atom/string concatenation

Predicates relating two stringys and their concatentation, and which also can split.
They throw away information about the type of the concatenated stringy, which is known
if it's the output, but can be arbitrary if it's the input.

- [atom_concat/3](https://eu.swi-prolog.org/pldoc/man?predicate=atom_concat/3) (an ISO-standard predicate)
- [string_concat/3](https://eu.swi-prolog.org/pldoc/man?predicate=string_concat/3)

```
?- atom_concat(a,b,X).
X = ab.

?- atom_concat(A,B,ab).
A = '', B = ab ;
A = a,  B = b ;
A = ab, B = ''.

?- string_concat(a,b,X).
X = "ab".

?- string_concat(A,B,ab).
A = "",   B = "ab" ;
A = "a",  B = "b" ;
A = "ab", B = "".
```

A predicate taking two _atomic_ terms as input and generating an atom as
their concatenation. This predicate cannot split because arguments 1 and 2 are too general,
they could be numbers for example.

- [atomic_concat/3](https://eu.swi-prolog.org/pldoc/man?predicate=atomic_concat/3)

```
?- atomic_concat(1,b,X).
X = '1b'.
```

Predicates taking a list of atomic terms as input and generating an atom or a string as
their concatenation. Again, these predicate cannot split.

- [atomic_list_concat/2](https://eu.swi-prolog.org/pldoc/man?predicate=atomic_list_concat/2) - generates atom at argument 2. Refuses string at argument 2 in accept mode (that's likely a bug).
- [atomics_to_string/2](https://eu.swi-prolog.org/pldoc/man?predicate=atomics_to_string/2) - generates string at argument 2. Refuses atom at argument 2 in accept mode (that's likely a bug).

```
?- atomic_list_concat([a,b,c],X).
X = abc.

?- atomics_to_string([a,2,c],X).
X = "a2c".
```

Predicates taking a list of concatenable atomic terms and a separator as input, and they
_intersperse_ the separator and the atomic terms into a concatenated atom or string.
These predicates can split at the separator.

- [atomic_list_concat/3](https://eu.swi-prolog.org/pldoc/man?predicate=atomic_list_concat/3) - concatenate with separator ("intersperse", "join"). Refuses string at argument 3 in accept mode (that's likely a bug).
- [atomics_to_string/3](https://eu.swi-prolog.org/pldoc/man?predicate=atomics_to_string/3) - concatenate with separator ("intersperse", "join"). Refuses atom at argument 3 in accept mode (that's likely a bug).

```
?- atomic_list_concat([a,b,c],'foo,X).
X = afoobfooc.

?- atomics_to_string(L,"foo","barfoobaz").
L = [bar,baz].
```
