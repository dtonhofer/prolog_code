# `stringy_morph.pl`

A "logical" mapper between atoms and SWI-Prolog strings and "list representations"
of character sequences, i.e. proper lists of chars and proper lists of codes.

This encapsulates/replaces the existing predicates [`atom_string/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_string/2),
[`atom_chars/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_chars/2),
[`string_chars/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=string_chars/2),
[`atom_codes/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom_codes/2) and
[`string_codes/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=string_codes/2).

- [`stringy_morph.pl`](stringy_morph.pl) (MIT license) 
- [`stringy_morph.plt`](stringy_morph.plt) (0BSD license)

The predicates avoid losing information or being imprecise about the types of their arguments 
by taking additional type information in separate arguments. If there is ambiguity in the types,
the predicates may provide additional solutions on redo. The predicate try to be deterministic
if there is only a single solution (took me some time to find the correct trick).

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

