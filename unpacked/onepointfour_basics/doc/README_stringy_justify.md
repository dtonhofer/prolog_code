# `stringy_justify.pl`

- [`stringy_justify.pl`](../stringy_justify.pl)
- [`stringy_justify.plt`](../stringy_justify.plt)

Justify strings inside a field of whitespace.

Those little predicates may be called a lot of times (e.g. when formatting
tables), so let's not become too slow when running it!
Currently, there are meta-calls in there and checks and everything, so the
code is not necessarily fast.

## Loading the module and running its tests (in SWI-Prolog)

Please refer to the [README.md](../README.md) file.

## Synopsis

One always wants to justify `Text` (a stringy, i.e. an atom or string)
inside a field of `Width` SPACE (0x20) characters, giving `Result`
(which can also be passed instantiated in case one wants to "accept"
`Result` instead of computing it).

- The resulting text will be cut off if it overflows the field of
  `Width` space.
- The resulting text will always be of `Width` with the SPACE
  character doing necessary padding.
- The type of `Result` (string or atom) corresponds to the type
  of `Text`.

```
justify_left(+Text,+Width,-Result)
justify_right(+Text,+Width,-Result)
justify_center(+Text,+Width,-Result)
```

The same as above, just with the `left`, `right`, `center`
passed as argument

```
justify_how(+How,+Text,+Width,-Result)
```

The same as above, only you can specify the type of the
`Result` with `ResultType` (it must be one of the atoms
`string` or `atom`).

- Additionally, the predicate entry checks can be skipped
  if `NoCheck` is set to true.

```
justify_left(+Text,+Width,-Result,?ResultType,+NoCheck)
justify_right(+Text,+Width,-Result,?ResultType,+NoCheck)
justify_center(+Text,+Width,-Result,?ResultType,+NoCheck)
```

The same as above, but an SWI-Prolog dict carries a number of additional flags:

```
justify(+How,+Text,+Width,-Result,?ResultType,+NoCheck,+Flags:Dict)
```

`Flags` may hold the following keys-value pairs

- cut_left  boolean (i.e. `true` or `false)
- cut_right boolean
- prefer
- offset
- no_trailing

## Examples


