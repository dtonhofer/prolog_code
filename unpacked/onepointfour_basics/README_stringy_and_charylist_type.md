# `stringy_and_charylist_type.pl`

Simple analysis for _stringy_ and _chary_ terms.

- [`stringy_and_charylist_type.pl`](stringy_and_charylist_type.pl)
- [`stringy_and_charylist_type.plt`](stringy_and_charylist_type.plt)

Vocabulary:

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

- `charylist_type(@CharyList,?Type`
- `stringy_type(@Stringy,?Type)`
- `stringy_type(@Stringy,?Type,@Throw)`
- `stringy_type_with_length(@Stringy,Type)`
- `stringy_type_with_length(Stringy,Type,Throw)`
- `stringy_length(@Stringy,?Length)`
- `stringy_length(@Stringy,?Length,@Throw)`

## Examples

```
?- charylist_type(_,T).
T = var.

?- charylist_type([],T).
T = empty.

?- charylist_type([h,e,l,l,o],T).
T = chars(5).

?- charylist_type([0'h,0'e,0'l,0'l,0'o],T).
T = codes(5).

?- charylist_type(foo,T).
false.

?- charylist_type([x],foo).
false.
```
  
```  
?- stringy_type(foo,T).
T = atom.

?- stringy_type("foo",T).
T = string.

?- stringy_type(_,T).
T = var.

?- stringy_type([h,e,l,l,o],T).
false.

?- stringy_type([h,e,l,l,o],T,throw).
ERROR: check failed : type error (the culprit is not of the required type)
ERROR:    message   : the value should fulfill 'stringy-ness'
ERROR:    culprit   : [h,e,l,l,o]
```

```
?- stringy_type_with_length("foo",T).
T = string(3).

?- stringy_type_with_length("",T).
T = string(0).

?- stringy_type_with_length('',T).
T = atom(0).

?- stringy_type_with_length(_,T).
T = var.
```

```
?- stringy_length("foo",T).
T = 3.

?- stringy_length(foo,T).
T = 3.

?- stringy_length(X,3).
ERROR: check failed : too_little_instantiation error (the culprit is not instantiated (enough))
ERROR:    message   : the value should fulfill 'nonvar-ness'
```
