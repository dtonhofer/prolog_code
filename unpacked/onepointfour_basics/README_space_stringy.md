# `space_stringy.pl`

Generate or accept SWI-Prolog strings or atoms made only of the _SPACE_ character (Unicode code point 0x20).

- [`space_stringy.pl`](space_stringy.pl) (MIT license)
- [`space_stringy.plt`](space_stringy.plt) (0BSD license)

This is a simple but still interesting exercise. The code uses the
functionality provided by `check.pl` to verify passed arguments and
fail or throw as required.

## Loading the module and running its tests (in SWI-Prolog)

Please refer to the [README.md](README.md) file.

## Synopsis

**`space_stringy(?N,?Stringy,?StringyType,@Tuned)`**

Provide any of the following:

   - `N`: an integer indicating the length of `Stringy`;
   - `Stringy`: the stringy (i.e. the string or atom) to either generate or accept; only made of SPACE on output;
   - `StringyType`: the type of `Stringy`, an atom that is either `atom` or `string`;
   - `Tuned`: input only: if `hard`, the predicate throws on negative `N`, if `soft` (actually, anything other than `hard`), it fails on negative `N`.

**`space_stringy(?N,?Stringy,+StringyType)`**

As `space_stringy/4`, with `Tuned` bound to `soft`.

**`space_stringy_smooth(?N,?Stringy,?StringyType)`**

As `space_stringy/4`, with `Tuned` bound to `soft`, but addtionally 
just fails on badly typed input instead of throwing (i.e. behaves "smoothly").

**`space_stringy_lax(?N,?Stringy,?StringyType)`**

As `space_stringy/4` but accepts negative `N`, generating the empty string or atom in that case.

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

Negative length fails:

```
?- space_stringy(-1,S,string).
false.
```

Unless you specify "hard":

```
?- space_stringy(-1,S,string,hard).
ERROR: check failed : domain error (the culprit is outside the required domain)
ERROR:    message   : the value should fulfill 'integer that is >= 0-ness'
ERROR:    culprit   : -1
```

The lax version generates zero-length stringys if length is negative:

```
?- space_stringy_lax(-1,S,string).
S = "".

?- space_stringy_lax(-1,S,Type).
S = '', Type = atom ;
S = "", Type = string.
```

Bad argument types result in exceptions:

```
?- space_stringy(foo,S,string).
ERROR: check failed : type error (the culprit is not of the required type)
ERROR:    message   : the value should fulfill 'integer-ness'
ERROR:    culprit   : foo
```

Unless you use the "smooth" version:

```
?- space_stringy_smooth(foo,S,string).
false.
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
