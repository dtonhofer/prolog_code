# `stringy_morph.pl`

A "logical" mapper between atoms and SWI-Prolog strings and "list representations"
of character sequences, i.e. proper lists of chars and proper lists of codes.

- [`stringy_morph.pl`](stringy_morph.pl) (MIT license)
- [`stringy_morph.plt`](stringy_morph.plt) (0BSD license)

The module provides a replacement for

- [`atom_string/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_string/2),

with a type-information taking/providing predicate `stringy_morph/4` which
restricts the arguments to strings and atoms.

It also provides a replacement for all of

- [`atom_codes/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_codes/2) and
- [`string_chars/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=string_chars/2),
- [`string_codes/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=string_codes/2).

with a type-information taking/providing predicate `stringy_charylist_morph/4`,
restricting arguments to strings and atoms for argument 1, and list of codes or chars on
argument 2.

The existing predicates are problematic because they try to be flexible
in what they accept (for backwards-compatibility reasons), taking atoms, strings,
or list representations (possibly on either of their two arguments). They also try
to be semi-deterministic, so they always provide at most one solution of a specifc type,
becoming non-logical:

One would expect this from the name only:

```
?- atom_string(hello,S).
S = "hello".

?- atom_string(S,"hello").
S = hello.
```

But this may be non-obvious:

```
?- atom_string(hello,hello).
true.

?- atom_string("hello",hello).
true.

?- atom_string("hello","hello").
true.
```

The above is in logical contradiction with getting only one solution here:

```
?- atom_string(hello,S).
S = "hello".
```

We shall demand that the caller provide type information regarding
what he expects on either of the two arguments to be morphed (in any direction).
If the type information is ambiguous (i.e. the argument is unbound), the predicate
shall propose a value and provide the correspondingly typed value generated from
any available input. If this means providing two solutions, so be it.

Thus for the new predicate:

**`stringy_morph(StringyA,StringyB,TypeOfStringyA,TypeOfStringB)`**

I want a _string_ at argument place 2, `StringyB`. I don't care to be told about the
type of argument 1, so I provide a `_` at argument place 3:

```
?- stringy_morph(hello,StringyB,_,string).
StringyB = "hello".
```

If I state that the type of argument 1 is a _string_, I get told otherwise:

```
?- stringy_morph(hello,StringyB,string,string).
false.
```

Because it's an _atom_:

```
?- stringy_morph(hello,StringyB,atom,string).
StringyB = "hello".
```

If I am lax in specifying the wanted output type, I get two solutions:

```
?- stringy_morph(hello,StringyB,_,Whatever).
StringyB = hello, Whatever = atom ;
StringyB = "hello", Whatever = string.
```

Of course, one can _accept_ a pair of arguments:

```
?- stringy_morph(hello,"string",atom,string).
true.
```

Or query their type, as long as they can be morphed from one to the other:

```
?- stringy_morph(hello,"hello",TypeA,TypeB).
TypeA = atom,
TypeB = string.
```

Similarly for the new predicate:

**`stringy_charylist_morph(Stringy,Charylist,StringyType,CharylistType)`**

The predicates try to be well-behaved deterministic if there is only a single solution
(took me some time to find the correct trick).

(Should we go further and pack all of the above into a single `texty_morph/4`? We could!!)

We also introduce the following additional vocabulary:

- A _stringy_ term is a term that is either an _atom_ or a _string_. In SWI-Prolog, the _string_
  is a distinct representation of a sequence of characters, distinct from the _atom_ and
  mean to be used in text processing rather than as basis for identifiers.
- A _chary_ term is a term that is either a _char_ (an atom of length 1) or a _code_ (an integer
  and, more precisely in SWI-Prolog, a Unicode code point).
- A _charylist_ is less precise: it is a proper list of either codes or chars. It may or may not contain
  uninstantiated elements. An empty list is a _charylist_ but we cannot know whether it is supposed
  to be composed of codes or chars. A list containing only uninstantiated variables is also a _charylist_
  and again we don't know what it is supposed to contain, at least not yet.

## Loading the module and running its tests (in SWI-Prolog)

Please refer to the [README.md](README.md) file.

## Synopsis

**Morphing between two stringys**

- `stringy_morph(StringyA,StringyB,TypeA,TypeB)` - preferentially fails on bad input
- `stringy_morph(StringyA,StringyB,TypeA,TypeB,Throw)` - can be told to throw on bad input with `Throw=true` or `Throw=false`

**Morphing between a stringy and a charylist**

- `stringy_charylist_morph(Stringy,Charylist,StringyType,CharylistType)` - preferentially fails on bad input
- `stringy_charylist_morph(Stringy,Charylist,StringyType,CharylistType,Throw)` - can be told to throw on bad input with `Throw=true` or `Throw=false`

## Examples

(Output made clearer manually relative to the on that SWI-prolog would print)

```
?-  stringy_morph(an_atom,StringyB,TypeA,TypeB).
StringyB = an_atom, TypeA = atom, TypeB = atom ;
StringyB = "an_atom", TypeA = atom, TypeB = string.

?- stringy_morph("a_string",StringyB,TypeA,TypeB).
StringyB = "a_string", TypeA = string, TypeB = string ;
StringyB = a_string, TypeA = string, TypeB = atom.

?- stringy_morph(StringyA,"a_string",TypeA,TypeB).
StringyA = "a_string", TypeA = string, TypeB = string ;
StringyA = a_string, TypeA = atom, TypeB = string.

?-  stringy_morph(StringyA,an_atom,TypeA,TypeB).
StringyA = an_atom, TypeA = atom, TypeB = atom ;
StringyA = "an_atom", TypeA = string, TypeB = atom.

?- stringy_morph(StringyA,an_atom,string,_).
StringyA = "an_atom".

?- stringy_morph(an_atom,StringyB,_,string).
StringyB = "an_atom".

?- stringy_morph(an_atom,StringyB,TypeA,string).
StringyB = "an_atom", TypeA = atom.

?- stringy_morph(an_atom,StringyB,TypeA,string).
StringyB = "an_atom", TypeA = atom.
```

```
?- stringy_charylist_morph("hello",Charylist,StringyType,CharylistType).
Charylist = [h,e,l,l,o], StringyType = string, CharylistType = chars ;
Charylist = [104,101,108,108,111], StringyType = string, CharylistType = codes.

?- stringy_charylist_morph(hello,Charylist,StringyType,CharylistType).
Charylist = [h,e,l,l,o], StringyType = atom, CharylistType = chars ;
Charylist = [104,101,108,108,111], StringyType = atom, CharylistType = codes.

?- stringy_charylist_morph(Stringy,[h,e,l,l,o],StringyType,CharylistType).
Stringy = hello, StringyType = atom, CharylistType = chars ;
Stringy = "hello", StringyType = string, CharylistType = chars.

?- stringy_charylist_morph(Stringy,[104,101,108,108,111],StringyType,CharylistType).
Stringy = hello, StringyType = atom, CharylistType = codes ;
Stringy = "hello", StringyType = string, CharylistType = codes.

?- stringy_charylist_morph("hello",Charylist,_,codes).
Charylist = [104,101,108,108,111].

?- stringy_charylist_morph("hello",[104,101,108,108,111],T1,T2).
T1 = string, T2 = codes.

?- stringy_charylist_morph(Stringy,[],T1,T2).
Stringy = '', T1 = atom, T2 = chars ;
Stringy = '', T1 = atom, T2 = codes ;
Stringy = "", T1 = string, T2 = chars ;
Stringy = "", T1 = string, T2 = codes.
```

