# `checks.pl`

A more powerful replacement for the venerable [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2).

- [`checks.pl`](checks.pl) (MIT license)
- [`checks.plt`](checks.plt) (0BSD license)

TODO:

I seems one sometimes wants to provide an explicit error message instead of having check_that construct a confusing one. Make that possible!

This page needs completion. For now, all the description is in [`checks.pl`](checks.pl).

## Check keywords implemented so far

"TBC" stands for "The term to be checked".

| Keyword                             | If TBC is var   | TBC must be a/an .... |
| :--                                 | :--             | :-- |
| `true`                              | doesn't care    | This check always succeeds, whatever TBC is. |
| `false`,`fail`                      | doesn't care    | This check always fails, whatever TBC is. |
| `random(Probability)`               | doesn't care    | This check randomly fails with probability 0 =< `Probability` =< 1. Useful for playing chaos monkey. |
| `var`                               | covered by test | uninstantiated term. |
| `nonvar`                            | covered by test | instantiated term. |
| `nonground`                         | covered by test | nonground term (and thus may be uninstantiated). |
| `ground`                            | covered by test | ground term (and thus will be instantiated). |
| `atom`,`symbol`                     | throws          | _atom_. |
| `atomic`,`constant`                 | throws          | _atomic_ term. |
| `compound`                          | throws          | _compound_ term. |
| `boolean`                           | throws          | one of the atoms `true` or `false`. |
| `pair`                              | throws          | compound term with functor name `-` and arity 2. |
| `string`                            | throws          | SWI-Prolog _string_. |
| `stringy`                           | throws          | either an _atom_ or an SWI-Prolog _string_. |
| `nonempty_stringy`                  | throws          | nonempty _stringy_: a _stringy_ that is different from `''` and `""`. |
| `char`                              | throws          | _atom_ of length 1. This is the traditional Prolog _char_ type. |
| `char_list`,`chars`                 | throws          | proper list of 0 or more _chars_. Unbound elements are not allowed. |
| `code`                              | throws          | _integer_ between 0 and 0x10FFFF (a Unicode code point). Detailed range checks are not done. |
| `code_list,codes`                   | throws          | proper list of 0 or more _codes_. Unbound elements are not allowed. |
| `chary`                             | throws          | _char_ or a _code_. |
| `chary_list`,`charys`               | throws          | proper list of 0 or more _chars_ or _codes_ (but consistently only one of those). Unbound elements are not allowed. |
| `stringy_typeid`                    | throws          | one of the atoms `string` or `atom`. Compare with the check `boolean`. |
| `chary_typeid`                      | throws          | one of the atoms `char` or `code`. Compare with the check `boolean`. |
| `number`                            | throws          | _number_ (encompasses _float_, _rational_, _integer_). |
| `float`                             | throws          | _float_ (64 bit double precision), including `+1.0Inf`, `-1.0Inf`, `NaN`, `-0.0`. |
| `float_not_nan`                     | throws          | _float_, excluding `NaN`. |
| `float_not_inf`                     | throws          | _float_, excluding `+1.0Inf`, `-1.0Inf`. |
| `float_not_neginf`                  | throws          | _float_, excluding `-1.0Inf`. |
| `float_not_posinf`                  | throws          | _float_, excluding `+1.0Inf`. |
| `int/integer`                       | throws          | _integer_. |
| `rational`                          | throws          | SWI-Prolog _rational, which encompasses _integer_. |
| `nonint_rational`,`proper_rational` | throws          | SWI-Prolog _rational_ that is not an _integer_. |
| `negnum`,`negnumber`                | throws          | strictly negative _number_. |
| `posnum`,`posnumber`                | throws          | strictly positive _number_. |
| `neg0num`,`neg0number`              | throws          | negative-or-zero _number_. |
| `pos0num`,`pos0number`              | throws          | positive-or-zero _number_. |
| `non0num`,`non0number`              | throws          | non-zero _number_. |
| `negint`,`negative_integer`         | throws          | strictly negative _integer_. |
| `posint`,`positive_integer`         | throws          | strictly positive _integer_. |
| `neg0int`                           | throws          | negative-or-zero _integer_. |
| `pos0int`,`nonneg`                  | throws          | positive-or-zero _integer_. |
| `negfloat`,                         | throws          | strictly negative _float_. | 
| `posfloat`                          | throws          | strictly positive _float_. |
| `neg0float`                         | throws          | negative-or-zero _float_. |
| `pos0float`                         | throws          | positive-or-zero _float_. |
| `inty`                              | throws          | _integer_ or a _float_ that represents an integer, e.g `1.0`. |
| `neginty`                           | throws          | strictly negative _inty_. | 
| `posinty`                           | throws          | strictly positive _inty_. | 
| `neg0inty`                          | throws          | negative-or-0 _inty_. | 
| `pos0inty`                          | throws          | positive-or-0 _inty_. | 
| `list`,`proper_list`                | throws          | proper list, including the empty list. (**TODO: open lists**) |
| `nonempty_list`                     | throws          | proper list that is not empty. |
| `dict`                              | throws          | SWI-Prolog _dict_ (which is a compound term following some special requirements). |
| `cyclic`                            | covered by test | term that has a cyclic structure. |
| `acyclic`                           | covered by test | term that has no cyclic structure _now_, but may acquire it _later_, unless the term is ground. |
| `acyclic_forever`                   | covered by test | term that is both ground and acyclic and will never become cyclic. |
| `stream`                            | throws          | term that is either "stream name" (certain _atoms_) or a valid _stream handle_ (certain _blobs_). |
| `unifies(Z)`                        | covered by test | term that unifies with `Z`. Unification is rolled back by use `\+ \+`. |
| `member(ListOfValues)`              | throws          | term that is member of the `ListOfValues`. Membership test is _unification_, as for `member/2`. |
| `forall(ListOfChecks)`              | depends         | TBC must pass all checks in `ListOfChecks`. Recursive. Same as writing the checks down normally, thus not really useful. |
| `forany(ListOfChecks)`              | depends         | TBC must pass at least one check in `ListOfChecks`. Recursive. Useful for implementing an `or`. |
| `fornone(ListOfChecks)`             | depends         | TBC must pass no check in `ListOfChecks`. Recursive. Useful for negation. |
| `passall(Check)`                    | throws          | `Check` is one of the check keywords. TBC must be a proper list. All the terms in that list must pass `Check`. Useful for terse code. |
| `passany(Check)`                    | throws          | `Check` is one of the check keywords. TBC must be a proper list. At least one terms in that list must pass `Check`. |
| `passnone(Check)`                   | throws          | `Check` is one of the check keywords. TBC must be a proper list. None of the terms in that list must pass `Check`. |

**Keywords provided by `must_be/2` not yet implemented**

- `between(FloatL,FloatU)`: if FloatL is float, all other values may be float or integer (FIXME?); the limits are both INCLUSIVE; limits may be equal but NOT reversed
- `between(IntL,IntU)`: if IntL is integer, all other values must be integer; the limits are both INCLUSIVE; limits may be equal but NOT reversed. FIXME: there should be specific between_int/2 and between_float/2 if one goes that way.
- `text`: atom or string or chars or codes (but not numbers even though some predicates "textify" those)               
- `list(Type)`: proper list with elements of type Type (must_be/2(Type,_) is called on elements); empty list is allowed; on error the index is not indicated. A type like "list(list(integer))" is ok! Actually corresponds to `passall(Type)`
- `list_or_partial_list`: A partial list (one ending in a variable: [x|_]). This includes an unbound variable.
- `callable`: passes callable/1. Relatively usesless, as "callable" is ill-defined. Basically (atom(X);compound(X))
- `encoding`: valid name for a character encoding; see current_encoding/1, e.g. utf8 (but not "utf8" or 'utf-8'; also fails for 'iso_8859_1')
- `type`: Meta: Term is a valid type specification for must_be/2. This is done by looking up whether a clause `has_type(Type,_) :- ....` exists. Example: must_be(type,list(integer)). However, "must_be(type,list(grofx))": OK, but "must_be(type,grofx)": NOT OK.

**Possible extension:**

- `predicate_indicator`: A Name/Arity predicate indicator
- `list_length_X`: Tests for length of lists (larger, smaller, equal)
- `subsumes`
- `does_not_unify` / `dif`
  
## Examples

Fail if `X` is not a _string_ (but throw if `X` is unbound): `check_that(X,[soft(string)])`

```
?- check_that(foo,[soft(string)]).
false.
```

Throw if `X` is not a _string_ (or unbound): `check_that(X,[hard(string)])`

```
?- check_that(foo,[hard(string)]).
ERROR: check failed : type error (the culprit is not of the required type)
ERROR:    message   : the value should fulfill 'string-ness'
ERROR:    culprit   : foo
```

Throw if `X` is not an _integer_ and then fail if `X` is not a _positive integer_: `check_that(X,[hard(int),soft(posint)])`

```
?- check_that(foo,[hard(int),soft(posint)]).
ERROR: check failed : type error (the culprit is not of the required type)
ERROR:    message   : the value should fulfill 'integer-ness'
ERROR:    culprit   : foo
```

```
?- check_that(-1,[hard(int),soft(posint)]).
false.
```

```
?- check_that(1,[hard(int),soft(posint)]).
true.
```

A type of test often done on predicate entry: break off with success if `X` is unbound, but
otherwise it must absolutely be _stringy_, and even a _nonempty stringy_ (for example):
`check_that(X,[break(var),hard(stringy),soft(nonempty_stringy)])`.

```
?- check_that(X,[break(var),hard(stringy),soft(nonempty_stringy)]).
true.
```

```
?- check_that(12,[break(var),hard(stringy),soft(nonempty_stringy)]).
ERROR: check failed : type error (the culprit is not of the required type)
ERROR:    message   : the value should fulfill 'stringy-ness'
ERROR:    culprit   : 12
```

```
?- check_that("",[break(var),hard(stringy),soft(nonempty_stringy)]).
false.
```

```
?- check_that("foo",[break(var),hard(stringy),soft(nonempty_stringy)]).
true.
```

## Smooth, soft, tuned, hard failure modes

Using the `tuned/1` tag, you can switch from "hard" to "soft" behaviour:

This behaves as if you had written `hard(posint)`:

```
check_that(X,[hard(int),tuned(posint)],throw)
```

This behaves as if you had written `soft(posint)`:

```
check_that(X,[hard(int),tuned(posint)],false)
```

Succeed if `X` is unbound, and then fail or throw depending on `Throw` if `X` is not a member of the given list:

```
check_that(X,[break(var),tuned(member([alpha,bravo,charlie]))],Throw).
```

Running it:

```
?- check_that(X,[break(var),tuned(member([alpha,bravo,charlie]))],throw).
true.
```

```
?- check_that(bar,[break(var),tuned(member([alpha,bravo,charlie]))],throw).
ERROR: check failed : domain error (the culprit is outside the required domain)
ERROR:    message   : the value should fulfill 'list_membership-ness'
ERROR:    culprit   : bar
```

```
?- check_that(bar,[break(var),tuned(member([alpha,bravo,charlie]))],false).
false.
```
