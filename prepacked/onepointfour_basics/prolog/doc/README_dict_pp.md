[Top README](../README.md)

# `dict_pp.pl`

SWI-Prolog dict prettyprinter.

- [`dict_pp.pl`](../dict_pp.pl) (MIT license)
   - [`dict_pp/decision.pl`](../dict_pp/decision.pl) (MIT license)
   - [`dict_pp/helpers.pl`](../dict_pp/helpers.pl) (MIT license)
   - [`dict_pp/lineify.pl`](../dict_pp/lineify.pl) (MIT license)
   - [`dict_pp/string_stuff.pl`](../dict_pp/string_stuff.pl) (MIT license)
   - [`dict_pp/string_stuff.plt`](../dict_pp/string_stuff.plt) (0BSD license)
   - [`dict_pp/topmost.pl`](../dict_pp/topmost.pl) (MIT license)
- [`dict_pp.plt`](../dict_pp.plt) (0BSD license)

See also:

- [SWI-Prolog dicts](https://eu.swi-prolog.org/pldoc/man?section=bidicts)
- [`format/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=format/2)

## Loading the module and running its tests (in SWI-Prolog)

Please refer to the [README.md](../README.md) file, but in short:

```
?- assertz(file_search_path(library,'/foo/bar/prolog_code/unpacked')).
?- use_module(library('onepointfour_basics/dict_pp.pl')).
?- load_test_files([]).
?- run_tests.
```

## Synopsis

### `dict_pp(+Dict)`

Prettyprint `Dict` directly using `format/2`, writing to the stream given by
`current_output`. An empty `Dict` does not lead to failure but to no output.

Wrap this goal with `with_output_to/2` to redirect the output to a stream of
your choice.

Behaves as `dict_pp/3` called with default settings, followed by immediate
printing of the resulting lines.

### `dict_pp(+Dict,+SettingsDict)`

Prettyprint `Dict` directly using `format/2`, writing to the stream given by
`current_output`. An empty `Dict` does not lead to failure but to no output.

Instructions on how to format the output can be given by `SettingsDict`. The
tag of that dict is arbitrary. Giving an empty dict here means that default
settings are used in all cases.

Wrap this goal with `with_output_to/2` to redirect the output to a stream of
your choice.

Behaves as `dict_pp/3`, followed by immediate printing of the resulting lines.

### `dict_pp(+Dict,+SettingsDict,-Result:list(string))`

Prettyprint `Dict` by generating strings that are accumulated into
the list of SWI-Prolog strings `Result`, for later emission to an output 
stream. The lines do not have a newline at their end. An empty `Dict` does 
not lead to failure but to an empty or nonempty list, depending on what can 
be found in `SettingsDict`.

Instructions on how to format the output are given via `SettingsDict`. The
tag of that dict is arbitrary.  Giving an empty dict here means that default
settings are used in all cases.

The following settings are understood. Anything not recognized is disregarded.
If a setting is missing when it is needed, the indicated default value is used.

| key                | value                           | default    | explainer |
| :-                 | :-                              | :-         | :- |
| `border`           | `true`, `false`                 | `false`    | Decorate outermost dict with an ASCII border. |
| `sub_border`       | `true`, `false`, `inherit`      | `inherit`  | Whether to decorate subdicts with an ASCII border, too. |
| `tag`              | `true`, `false`                 | `true`     | Print the tag of the outermost dict. If the tag is an unbound variable, it is not printed. |
| `sub_tag`          | `true`, `false`, `inherit`      | `inherit`  | Whether to display the tags of subdicts, too. |
| `justify_key`      | `left`, `right`, `center`       | `left`     | How to justify the keys inside the key column. |
| `justify_value`    | `left`, `right`, `center`       | `left`     | How to justify the values inside the values column. |
| `justify_tag`      | `left`, `right`, `center`       | `center`   | How to justify the tag inside the tag line. |
| `justify_tag_full` | `true`, `false`                 | `true`     | Left and right padding is considered as being part of the tag field. |
| `spec_float`       | see `format/2`                  | `f`        | A `format/2` specifier used for floats. Passed to `format/3` "as is". |
| `spec_int`         | see `format/2`                  | `d`        | A `format/2` specifier used for integers. Passed to `format/3` "as is". |
| `depth_limit`      | int >= 0                        | 10         | "subdict depth" at which prettyprinting switches to a "one-liner". 0 means even the root dict is printed as a oneliner. |
| `pad`              | `true`, `false`                 | `false`    | Switch on padding according to `pad_left` etc. Note that if `pad` is `true` (and `border` is `false`), and none of the `pad_X` values has been given, then the output is a rectangle filled to rectangle-ness with whitespace. |
| `sub_pad`          | `true`, `false`, `inherit`      | `inherit`  | Whether to pad subdicts with whitespace, too. |
| `pad_left`         | int >= 0                        | 0          | Pad with whitespace on the left depending on `pad` and `sub_pad` (inside the ASCII border if any). |
| `pad_right`        | int >= 0                        | 0          | As above, on the right. |
| `pad_top`          | int >= 0                        | 0          | As above, on top (underneath the tag, if any).  |
| `pad_bottom`       | int >= 0                        | 0          | As above, on the bottom. |

## History

   1. 2021-01-29 - Version 1, a bit rough.
   1. 2021-02-05 - Version 2, works but unpackageable
   1. 2021-06-22 - Version 3, rebuild, passes all tests! For some reason, this ended up being a _lot_ of work.

## Examples

Prettyprint _Dict_ to a list of strings _Result_ with default settings (_SettingsDict_ set to `_{}`).
Note that here, _Dict_ has no valid tag.

```
?- dict_pp(_{a:1,b:2},_{},Result).
Result = ["a : 1","b : 2"].
true.
```

```
?- dict_pp(_{a:"hello world",b:"foo bar baz"},_{},Result).
Result = ["a : hello world","b : foo bar baz"].
true.
```

Immediate output of result, with some settings.

```
?- dict_pp(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},
           _{justify_key:right,spec_float:f}).
   w : 0.259848
  ww : 1.458760
 www : 643764856
wwww : 400
true.
```

Immediate output of result, with different settings. Here, _Dict_ has a valid tag.

```
?- dict_pp(various{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},
           _{justify_key:right,justify_value:right,spec_float:e}).
    various
   w : 2.598476e-01
  ww : 1.458760e+00
 www :    643764856
wwww :          400
true.
```

Wrap the result in a border.

```
?- dict_pp(various{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},
           _{border:true}).
+----------------+
|    various     |
+----------------+
|w    : 0.259848 |
|ww   : 1.458760 |
|www  : 643764856|
|wwww : 400      |
+----------------+
true.
```

No border, but with padding around the result.

```
?- dict_pp(various{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},
           _{pad_left:5,pad_right:4,pad:true}).
         various
     w    : 0.259848
     ww   : 1.458760
     www  : 643764856
     wwww : 400
true.
```

With both border _and_ padding.

```
?- dict_pp(various{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},
           _{pad:true,pad_left:2,pad_top:1,pad_bottom:1,pad_right:2,
             border:true,justify_tag:left,justify_tag_full:false}).
+--------------------+
|  various           |
+--------------------+
|                    |
|  w    : 0.259848   |
|  ww   : 1.458760   |
|  www  : 643764856  |
|  wwww : 400        |
|                    |
+--------------------+
true.
```

Prettyprint a dict with subdicts.

```
?- dict_pp(alpha{w1: 10, w2: 200, w3: 3000,
                   w4: bravo{w1: 10, w2: 20,
                      w3: charlie{ a: 12, b: 13}}},
           _{border:true}).
+---------------------+
|        alpha        |
+---------------------+
|w1 : 10              |
|w2 : 200             |
|w3 : 3000            |
|w4 : +--------------+|
|     |    bravo     ||
|     +--------------+|
|     |w1 : 10       ||
|     |w2 : 20       ||
|     |w3 : +-------+||
|     |     |charlie|||
|     |     +-------+||
|     |     |a : 12 |||
|     |     |b : 13 |||
|     |     +-------+||
|     +--------------+|
+---------------------+
true.
```

Prettyprint a dict with subdicts, but suppress the tags.


```
?- dict_pp(alpha{w1: 10, w2: 200, w3: 3000,
                   w4: bravo{w1: 10, w2: 20,
                      w3: charlie{ a: 12, b: 13}}},
           _{border:true,tag:false}).
+--------------------+
|w1 : 10             |
|w2 : 200            |
|w3 : 3000           |
|w4 : +-------------+|
|     |w1 : 10      ||
|     |w2 : 20      ||
|     |w3 : +------+||
|     |     |a : 12|||
|     |     |b : 13|||
|     |     +------+||
|     +-------------+|
+--------------------+
true.
```

Prettyprint a dict with subdicts, show the tags, don't show a border.

```
?- dict_pp(alpha{w1: 10, w2: 200, w3: 3000,
                   w4: bravo{w1: 10, w2: 20,
                      w3: charlie{ a: 12, b: 13}}},
           _{border:false,tag:true}).
      alpha
w1 : 10
w2 : 200
w3 : 3000
w4 :    bravo
     w1 : 10
     w2 : 20
     w3 : charlie
          a : 12
          b : 13
```



