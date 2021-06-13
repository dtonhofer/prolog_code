# `stringy_overwrite.pl`

- [`stringy_overwrite.pl`](stringy_overwrite.pl) (MIT license) 
- [`stringy_overwrite.plt`](stringy_overwrite.plt) (0BSD license)

## Synopsis

- Overwriting is done using "runs" (i.e. strings), not character-by-character.
- A "stringy" is an atom or string.
- A "stringy_typeid" is one of the atoms `atom` or `string`.

Default:

```
overwrite(+BgText:stringy,
          +FgText:stringy,
          +FgPos:integer,
          +CutLeft:boolean,
          +CutRight:boolean,
          -Result:stringy,
          ?ResultType:stringy_typeid)
```

Explicitly selecting processing by "chars" or "runs":

```
overwrite_using_runs(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType)  
```

```
overwrite_using_chars(BgText,FgText,FgPos,CutLeft,CutRight,Result,ResultType)
```

## Description

The predicate `overwrite/7`, described as 

```
overwrite(+BgText,+FgText,+FgPos,+CutLeft,+CutRight,?Result,+ResultType)
```

is predicate which is given a "background text" (atom or string), a "foreground text" (atom or string)
and the position of the foreground text relative to the background text. It creates
new text by overwriting the background text with the foreground text starting at the
relative position. Booleans indicate whether the result should be cut at the end and the start of
the foreground string, or not.

Graphically:

```
Foreground text:              |(computation is like maxing out your credit card)|
Background text:    |The universe can still end in the time you're computing the answer.|
Relative position              ^
                               +-- 10
Result:             |The univer(computation is like maxing out your credit card) answer.|
```

The call would be:

```
overwrite(
   "The universe can still end in the time you're computing the answer.",
   "(computation is like maxing out your credit card)",
   10,
   false,false,Result,_). % don't cut left, don't cut tight, deduce output type from input type
```

You can go out of bounds on both sides of "background text":

```
Foreground text:                                 |(computation is like maxing out your credit card)|
Background text:    |The universe can still end in the time you're computing the answer.|
Relative position                                 ^
                                                  +-- 29
Result:             |The universe can still end in(computation is like maxing out your credit card)|
```

The call would be:

```
overwrite(
   "The universe can still end in the time you're computing the answer.",
   "(computation is like maxing out your credit card)",
   29,
   false,false,Result,_). % don't cut left, don't cut tight, deduce output type from input type
```

Similarly for the negative side:

```
Foreground text:      |(computation is like maxing out your credit card)|
Background text:               |The universe can still end in the time you're computing the answer.|
Relative position      ^
                       +-- -9

Result:               |(computation is like maxing out your credit card)ou're computing the answer.|
```

Or you can "cut" on either side:

```
Foreground text:                                 |(computation is like maxing out your credit card)|
Background text:    |The universe can still end in the time you're computing the answer.|
Relative position                                 ^
                                                  +-- 29
Result:             |The universe can still end in(computation is like maxing out your c|
```

The call would be:

```
?- overwrite(
   "The universe can still end in the time you're computing the answer.",
   "(computation is like maxing out your credit card)",
   29,
   true,true,Result,_).
Result = "The universe can still end in(computation is like maxing out your c".
```

Similarly:

```
Foreground text:      |(computation is like maxing out your credit card)|
Background text:               |The universe can still end in the time you're computing the answer.|
Relative position      ^
                       +-- -9

Result:                        |ion is like maxing out your credit card)ou're computing the answer.|
```

There are two implementations for `overwrite/7`:

- `overwrite_using_chars/7` using lists of characters. The implementation is easy to check but slow.
- `overwrite_using_runs/7` using SWI-Prolog strings. The implementation is difficult to check but fast. It is the one used by `overwrite/7`

The implementation which employs lists of characters exists solely to compare its results
against the one using strings.

## Loading the module and running its tests (in SWI-Prolog)

Please refer to the [README.md](README.md) file.

