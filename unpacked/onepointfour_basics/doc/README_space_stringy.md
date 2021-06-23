# `space_stringy.pl`

Generate or accept SWI-Prolog strings or atoms made only of the _SPACE_ character (Unicode code point 0x20).

- [`space_stringy.pl`](../space_stringy.pl) (MIT license)
- [`space_stringy.plt`](../space_stringy.plt) (0BSD license)

This is a simple but still interesting exercise. The code uses the
functionality provided by `check.pl` to verify passed arguments and
fail or throw as required.

## Loading the module and running its tests (in SWI-Prolog)

Please refer to the [README.md](../README.md) file.

## Synopsis

- `space_stringy(?N,?Stringy,?StringyType,@Tuned)`
- `space_stringy(?N,?Stringy,+StringyType)`
- `space_stringy_smooth(?N,?Stringy,?StringyType)`
- `space_stringy_lax(?N,?Stringy,?StringyType)`

### More precisely

`space_stringy(?N,?Stringy,?StringyType,@Tuned)`

- Provide any of the following:
   - `N`: an integer indicating the length of `Stringy`;
   - `Stringy`: the stringy (i.e. the string or atom) to either generate or accept; only made of SPACE on output;
   - `StringyType`: the type of `Stringy`, an atom that is either `atom` or `string`;
   - `Tuned`: input only: if `hard`, the predicate throws on negative `N`, if `soft` (actually, anything other than `hard`), it fails on negative `N`.

`space_stringy(?N,?Stringy,+StringyType)`

- As `space_stringy/4`, with `Tuned` bound to `soft`, which means we just fail if `N` is negative.
  One would generally use this predicate, and use `space_stringy(?N,?Stringy,+StringyType,hard)`
  in specific places.

`space_stringy_smooth(?N,?Stringy,?StringyType)`

- As `space_stringy/4`, with `Tuned` bound to `soft`, but addtionally
  just fail on badly typed input (e.g. passing an atom as `N`) instead of throwing (i.e. the
  predicates behaves "smoothly", which is mostly not what one wants).

`space_stringy_lax(?N,?Stringy,?StringyType)`

- As `space_stringy/4` but accepts negative `N`, generating the empty string or atom in that case.
  This can be useful to avoid annoying tests for special cases.

## Examples

Give me a string of length 10:

```
?- space_stringy(10,S,string).
S = "          ".
```

Give me an atom of length 10:

```
?- space_stringy(10,S,atom).
S = '          '.
```

Give me the two space stringys (atom and string) of length 10:

```
?- space_stringy(10,S,Type).
S = '          ',Type = atom ;
S = "          ",Type = string.
```

Determine length (and also type):

```
?-` space_stringy(N,"    ",Type).
N = 4, Type = string.
```

This is not a stringy made of SPACE:

```
?- space_stringy(N," X  ",Type).
false.
```

This is a string, not an atom:

```
?- space_stringy(N," ",atom).
false.

?- space_stringy(N," ",string).
N = 1.
```

Generate them all:

```
?- space_stringy(N,Stringy,Type).
N = 0, Stringy = '', Type = atom ;
N = 0, Stringy = "", Type = string ;
N = 1, Stringy = ' ', Type = atom ;
N = 1, Stringy = " ", Type = string ;
N = 2, Stringy = '  ', Type = atom ;
N = 2, Stringy = "  ", Type = string ;
N = 3, Stringy = '   ', Type = atom ;
N = 3, Stringy = "   ", Type = string ;
N = 4, Stringy = '    ', Type = atom ;
N = 4, Stringy = "    ", Type = string ;
...
```

Bad input:

Negative length fails by default:

```
?- space_stringy(-1,S,string).
false.
```

Unless you explicitly set the additional argument `Throw` to `hard`:

```
?- space_stringy(-1,S,string,hard).
ERROR: check failed : domain error (the culprit is outside the required domain)
ERROR:    message   : the value should fulfill 'integer that is >= 0-ness'
ERROR:    culprit   : -1
```

As usual, it is difficult to assess whether the default should be the "hard" or the "soft" version

Passing `soft` as the additional argument `Throw` recovers the default behaviour:

```
?- space_stringy(-1,S,string,soft).
false.
```

The lax version generates zero-length stringys if length is negative:

```
?- space_stringy_lax(-1,S,string).
S = "".

?- space_stringy_lax(-1,S,Type).
S = '', Type = atom ;
S = "", Type = string.
```

Bad argument types result in exceptions by default:

```
?- space_stringy(foo,S,string).
ERROR: check failed : type error (the culprit is not of the required type)
ERROR:    message   : the value should fulfill 'integer-ness'
ERROR:    culprit   : foo
```

But the "smooth" version just fails if out-of-type arguments are provided. This
is generally not what one wants because the predicate should just not be called this way:

```
?- space_stringy_smooth(foo,S,string).
false.
```

## Thoughts

Should the selection for "lax" and "smooth" not be done via the name of the predicate but overloaded into the `Tuned` selector?
Maybe, maybe not.

For some reason SWI-Prolog 8.3 had trouble getting determinism out of space_string_4/3 on the first argument:

```
?- space_stringy(3,S,string).
S = "   ".

?- space_stringy(3,S,atom).
S = '   ' ;
false.
```

so I had to add a decision and a cut to get:

```
?- space_stringy(3,S,string).
S = "   ".

?- space_stringy(3,S,atom).
S = '   '.
```

## Notes: Ad-hoc methods to generate strings of SPACE

Another way of "generating strings of SPACE" is:

```
length(Codes, N),
maplist(=(0'\s), Codes),    % N space character codes
string_codes(Codes, String).
```

Or even like this, which is somewhat hard to remember:

```
format(string(String), '~t~*|', [2]).  % two space characters
```

However, the performance of of the first trick is much worse than the code
of `space_string.pl`. The `format/2` trick has about the same performance.

See `perf/space_string_performance.plt` for performance testing.

Another way of accepting a string made only of SPACE is:

```
string_codes(Codes, String),
maplist(=(0'\s), Codes)
```

Or even like this:

```
split_string(String, "", " ", [""]).
```

See also this code concerning "exponentiation" of an associative operation:

https://swi-prolog.discourse.group/t/power-implementation/1937

## Is it fast?

Not particularly, if the `check_that` calls are fully active. 

There is a simple performance test in the "perf" directory.

- "goal_special" uses `space_stringy(Length,Spaces,string)`
- "goal-direct" uses `length(Codes,Length),maplist(=(0'\s),Codes),string_codes(Codes,Spaces)`
- "goal_format" uses `format(string(Spaces),"~t~*|",[Length])`

There are two tests: create space strings of random length and drop them immediately.
Or else create space strings of random length and keep them in a list.

We find:

```
drop them immediately (100000 calls) (500 max size) using goal 'goal_special'
CPU time: 3.73 s, KiloInferences: 19202, Wallclock: 3.74 s


drop them immediately (100000 calls) (500 max size) using goal 'goal_direct'
CPU time: 1.8 s, KiloInferences: 26112, Wallclock: 1.81 s


drop them immediately (100000 calls) (500 max size) using goal 'goal_format'
CPU time: 1.11 s, KiloInferences: 800, Wallclock: 1.11 s

.
collect in list (100000 calls) (500 max size) using goal 'goal_special'
CPU time: 5.88 s, KiloInferences: 20308, Wallclock: 5.91 s


collect in list (100000 calls) (500 max size) using goal 'goal_direct'
CPU time: 3.44 s, KiloInferences: 27229, Wallclock: 3.46 s


collect in list (100000 calls) (500 max size) using goal 'goal_format'
CPU time: 1.56 s, KiloInferences: 1900, Wallclock: 1.56 s
```

However, if at least the full syntax check on the `check_that` calls is disabled, performance becomes competitive:

```
drop them immediately (100000 calls) (500 max size) using goal 'goal_special'
CPU time: 1.85 s, KiloInferences: 11001, Wallclock: 1.85 s


drop them immediately (100000 calls) (500 max size) using goal 'goal_direct'
CPU time: 1.8 s, KiloInferences: 26076, Wallclock: 1.8 s


drop them immediately (100000 calls) (500 max size) using goal 'goal_format'
CPU time: 1.11 s, KiloInferences: 800, Wallclock: 1.11 s

.
collect in list (100000 calls) (500 max size) using goal 'goal_special'
CPU time: 2.5 s, KiloInferences: 12103, Wallclock: 2.51 s


collect in list (100000 calls) (500 max size) using goal 'goal_direct'
CPU time: 2.86 s, KiloInferences: 27150, Wallclock: 2.87 s


collect in list (100000 calls) (500 max size) using goal 'goal_format'
CPU time: 1.55 s, KiloInferences: 1900, Wallclock: 1.56 s
```

