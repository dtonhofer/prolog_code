[Top README](../README.md)

# `stringy_justify.pl`

- [`stringy_justify.pl`](../stringy_justify.pl) (MIT license)
- [`stringy_justify.plt`](../stringy_justify.plt) (0BSD license)

Justify strings inside a field of given width supposed to contain only SPACE
(ASCII 0x20) characters.

## Loading the module and running its tests (in SWI-Prolog)

Please refer to the [README.md](../README.md) file, but in short:

```
?- assertz(file_search_path(library,'/foo/bar/prolog_code/unpacked')).
?- use_module(library('onepointfour_basics/stringy_justify.pl')).
?- load_test_files([]).
?- run_tests.
```

## Synopsis

```
justify_left(+Text:stringy,+FieldWidth:integer,-Result:stringy,-ResultType:atom)
justify_right(+Text:stringy,+FieldWidth:integer,-Result:stringy,-ResultType:atom)
justify_center(+Text:stringy,+FieldWidth:integer,-Result:stringy,-ResultType:atom)
```

One always wants to justify `Text` (a "stringy", i.e. an atom or string)
inside a field of `Width` SPACE characters, giving `Result`. `Result`
can also be passed instantiated in case one wants to "accept" `Result`
instead of generating it. The actual type of the `Result` is given by
`ResultType`: one of the atoms `atom` or `string`.

The resulting text will always be of `Width` (>= 0) with the SPACE character 
doing required padding. Text that overflows out of the field of `Width` SPACE
is cut off by default, but that can be configured.

A variation on the above is:

```
justify_how(+How:atom,+Text:stringy,+FieldWidth:integer,-Result:stringy,-ResultType:atom)
```

which allows you to specify how to justifiy via a parameter instead of via the functor name:
Set `How` to one of `left`, `right`, `center`.

For finetuning, there are predicates which take an additional `SettingDict` SWI-Prolog dict:

```
justify_left(+Text:stringy,+FieldWidth:integer,-Result:stringy,-ResultType:atom,+SettingsDict:dict)
justify_right(+Text:stringy,+FieldWidth:integer,-Result:stringy,-ResultType:atom,+SettingsDict:dict)
justify_center(+Text:stringy,+FieldWidth:integer,-Result:stringy,-ResultType:atom,+SettingsDict:dict)
justify_how(+How:atom,+Text:stringy,+FieldWidth:integer,-Result:stringy,-ResultType:atom,+SettingsDict:dict)
```

The following key-value pairs are recognized:

| key                | value                           | default    | explainer |
| :-                 | :-                              | :-         | :- |
| `cut_left`         | `true`, `false`                 | `true`     | Cut off overflowing text on the left |
| `cut_right`        | `true`, `false`                 | `true`     | Cut off overflowing text on the right |
| `offset`           | integer                         | 0          | Additional padding to be added (or subtracted) on the left in case of "left" justification, and to be added (or subtracted) on the right in case of "right" justification | 
| `offset_left`      | integer                         | 0          | Additional padding to be added (or subtracted) on the left in case of "center" justification | 
| `offset_right`     | integer                         | 0          | Additional padding to be added (or subtracted) on the right in case of "center" justification | 
| `prefer`           | `rightly`, `leftly`             | `leftly`   | In the case of `center` justification, there may be amibuity as to whether to shift the text to the left or to the right by a single character. This setting influences the decision. Esoteric. |


## Examples

```
?- justify_how(right,10,"hello",Result,string).
Result = "     hello".
```

```
?- justify_how(left,10,"hello",Result,string).
Result = "hello     ".
```

```
?- justify_how(center,10,"hello",Result,string).
Result = "  hello   ".
```

```
?- justify_how(right,10,"hello",Result,string,_{offset:3}).
Result = "  hello   ".
```

```
?- justify_how(right,15,"hello",Result,string,_{offset:3}).
Result = "       hello   ".
```

```
?- justify_how(right,15,"hello world this is a long text",Result,string,_{offset:3}).
Result = " a long text   ".
```
