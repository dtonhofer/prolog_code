# `stringy_morph.pl`

A "logical" mapper between atoms and SWI-Prolog strings and "list representations"
of character sequences, i.e. proper lists of chars and proper lists of codes.

This replaces the existing predicates [`atom_string/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_string/2),
[`atom_chars/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_chars/2),
[`string_chars/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=string_chars/2),
[`atom_codes/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_codes/2) and
[`string_codes/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=string_codes/2).

- [`stringy_morph.pl`](stringy_morph.pl) (MIT license) 
- [`stringy_morph.plt`](stringy_morph.plt) (0BSD license)

The predicates avoid losing information or being imprecise about the types of their arguments 
by taking additional type information in separate arguments. If there is ambiguity in the types,
the predicates may provide additional solutions on redo.

We introduce the following additional vocabulary:

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


