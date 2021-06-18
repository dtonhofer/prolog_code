# `space_string.pl`

Create or accept strings made only of the _SPACE_ character (Unicode code point 0x20).

- [`space_string.pl`](space_string.pl) (MIT license)
- [`space_string.plt`](space_string.plt) (0BSD license)

This is a simple but still interesting exercise. The code uses the
functionality provided by `check.pl` to verify passed arguments and
fail or throw as required.

## Loading the module and running its tests (in SWI-Prolog)

Please refer to the [README.md](README.md) file.

## Synopsis

- `space_string_smooth(?N,?Stringy)` - exclusively fails on bad input
- `space_string(?N,?String)` - mostly fails on bad input;
- `space_string(?N,?String,@Throw)` - with `Throw=throw` or `Throw=true`, prefers to throw on bad input;

## Examples

```
?- space_stringy(10,String,string).
String = "          ".

?- space_string(N,"    ").
N = 4.

?- space_string(N," hey  ").
false.

?- space_string(-1,S).
false.

?- space_string(-1,S,throw).
ERROR: check failed : domain error (the culprit is outside the required domain)
```

## Notes: Alternatives

Another way of "generating strings of spaces" is:

```
length(Codes, N),
maplist(=(0'\s), Codes),    % N space character codes
string_codes(Codes, String).
```

Or even like this, which is somewhat hard to remember

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
