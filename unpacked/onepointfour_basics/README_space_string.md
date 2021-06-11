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
?- space_string(10,String).
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
