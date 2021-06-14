# `checks.pl`

A more powerful replacement for the venerable [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2).

- [`checks.pl`](checks.pl) (MIT license)
- [`checks.plt`](checks.plt) (0BSD license)

## Synopsis

Check that term `X` fulfills all the conditions in the list `Conditions`. 
Conditions that are marked `tuned` will preferentially fail instead of throwing
if the condition is not fulfilled:

```
check_that(+X,@Conditions)
```

Check that term `X` fulfills all the conditions in the list `Conditions`.
If `Throw` is instantiated to `true` or `throw`, conditions that are marked `tuned` 
will preferentially throw instead of failing if the condition is not fulfilled (any
other state of `Throw`, including lack of instantiation, will yield the behaviour
of `check_that/2`):

```
check_that(+X,@Conditions,@Throw)
```

Same as `check_that/2`, but a `Name` for `X` is given. This name will be used when
the message for an excpetion is constructed:

```
check_that_named(X,Conditions,Name)
```

Same as `check_that/3`, but a `Name` for `X` is given. This name will be used when
the message for an excpetion is constructed:

```
check_that_named(X,Conditions,Name,Throw)
```       

### Exception terms

The exception terms thrown by `check_that/N` and `check_that_named/N` are not ISO exception terms, although they
still retain the outer `error(Formal,Context)` structure. They are structured as follows:

```       
error(check(Type,Expected,Msg,Culprit),_).
```       

Where `Type` is an exception-identifying atom that is generally one of:

- `type` - the culprit is of the wrong type to pass the check
- `domain` - the culprit is of the correct type but of the wrong domain to pass the check
- `uninstantiation` - the culprit is too instantiated (generally, fully instantiated when it shouldn't be)
- `instantiation` - the culprit is not instantiated enough (generally, fully uninstantiated when it shouldn't be)

and 

- `Expected` - either an uninstantiated term or a string that explains what is expected
- `Msg` - either an uninstantaited term or a string giving additional information
- `Culprit` - the term that caused the exception to be thrown; may be large or contain sensitive information!

A hook into `prolog:error_message/1` formats the exception for the toplevel printer.         
                  
## Description

`check_that/3` and friends: a replacement for the [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2) predicate 
of (SWI-)Prolog. `must_be/2` is used to check preconditions on predicate entry, but is not very flexible. Can we change that?

A call to check_that/3 looks as follows:

```
check_that(X,Conditions,Throw)
```

where

- `X` is the term that is subject to being checked.
- `Conditions` is a proper list of conditions to be evaluated left-to-right
- `Throw` is a flag that determines whether to, in certain settings, preferentially throw (if it is `true` or `throw`)
  or fail (if it is anything else, including unbound)

The simpler

```
check_that(X,Conditions)
```

assumes that Throw is =|false|=.

The more extensive

```
check_that_named(X,Conditions,Name)
check_that_named(X,Conditions,Name,Throw)
```

also take a `Name` to designate the `X` that is being checked. This `Name` can then
be inserted into exception messages. Generally one would not bother with this.

**TODO:**

I seems one sometimes wants to provide an explicit error message instead of having check_that construct a confusing one. Make that possible!

## The list of conditions

The list of _conditions_ in the call `check_that(X,Conditions)` behaves like a 
"conditional and" (aka. "short-circuiting and") of verifications. 

A _condition_ is a compound term, a tagged _check_. The functor name
of the condition specifies what to do depending on the outcome of the check.

The behaviour is as given below for the various tags allowed in conditions.

The "check precondition fails" generally means that the actual check cannot determine from the current
state of X whether to reasonably succeed or fail because X is not instantiated enough. The check would
then raise an instantiation exception (in the current implementation, a non-ISO exception term
`error(check(instantiation,_,_,_),_)`.

The Prolog default checks like `atom/1` take the high way and fail if an uninstantaited term is
passed. However, `atom(_)` failing means that either it pretends to know something about `_`
that it doesn't (namely that this is not an atom) or it is actually a second-order predicate
like `var/1` that can analyze the momentary state of the computation and say that a term is indeed
uninstantiated. In either case, we have something dubious.

`break/1`

- Precondition fails: An exception (generally a 'uninstantiated error' exception) is thrown.
- Verification fails: The condition succeeds, leading to examination of the next condition to the right.
- Verification succeeds: Condition processing stops and check_that succeeds overall

`smooth/1`

- Precondition fails: The condition fails, leading to the whole of check_that/N failing.
  This is like the behaviour of prolog predicates like atom/N when they are given an uninstantiated term: They just fail.
- Verification fails: The condition fails, leading to the whole of check_that/N failing.
- Verification succeeds: The condition succeeds, leading to examination of the next condition to the right.

`soft/1`

- Precondition fails: An exception (generally a 'uninstantiated error' exception) is thrown.
- Verification fails: The condition fails, leading to the whole of check_that/N failing.
- Verification succeeds: The condition succeeds, leading to examination of the next condition to the right.

`tuned/1` and the "Throw" flag is unset: behaves like `soft/1`

- Precondition fails: An exception (generally a 'uninstantiated error' exception) is thrown.
- Verification fails: The condition fails, leading to the whole of check_that/N failing.
- Verification succeeds: The condition succeeds, leading to examination of the next condition to the right.

`tuned/1` and the "Throw" flag is set: behaves like `hard/1`

- Precondition fails: An exception (generally a 'uninstantiated error' exception) is thrown.
- Verification fails: An exception (generally a 'type error' if X is out-of-type, and a domain error if X is 'out of domain') is thrown.
- Verification succeeds: The condition succeeds, leading to examination of the next condition to the right.

`hard/1`

- Check precondition fails: An exception (generally a 'uninstantiated error' exception) is thrown.
- Check verification fails: An exception (generally a 'type error' if X is out-of-type, and a domain error if X is 'out of domain') is thrown.
- Verification succeeds: The condition succeeds, leading to examination of the next condition to the right.

## Check keywords implemented so far

"TBC" stands for "The term to be checked".

| Keyword                             | If TBC is var   | TBC must be a/an .... |
| :--                                 | :--             | :-- |
| `true`                              | doesn't care    | This check always succeeds, whatever TBC is. |
| `false`,`fail`                      | doesn't care    | This check always fails, whatever TBC is. |
| `random(Probability)`               | doesn't care    | This check randomly fails with probability 0 =< `Probability` =< 1.<br>Useful for playing chaos monkey. |
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
| `code`                              | throws          | _integer_ between 0 and 0x10FFFF (a Unicode code point).<br>Detailed range checks are not done. |
| `code_list,codes`                   | throws          | proper list of 0 or more _codes_. Unbound elements are not allowed. |
| `chary`                             | throws          | _char_ or a _code_. |
| `chary_list`,`charys`               | throws          | proper list of 0 or more _chars_ or _codes_ (but consistently only one of those).<br>Unbound list elements cause an exception. |
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
| `cyclic`                            | covered by test | term that has a cyclic structure _now_.<br>An unbound TBC leads to failure (if `soft/1`) |
| `acyclic_now`                       | covered by test | term that has no cyclic structure _now_, but may acquire it _later_, unless the term is ground.<br>`check_that(_,hard(acyclic_now)).` succeeds. |
| `acyclic_forever`                   | covered by test | term that is both ground and acyclic and will never become cyclic. |
| `stream`                            | throws          | term that is either "stream name" (certain _atoms_) or a valid _stream handle_ (certain _blobs_).<br>The known stream names are `user_input`, `user_output`, `user_error`, `current_input`, `current_output`. |
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

## Usage scenarios:

  - Use check_that/2 to verify predicate entry preconditions
    - Expecting to switch them off at "production time" (assertions) to gain performance
    - They are also goo "live documentation" saying exactly what parts of the input space are covered by throws and which ones by fails
      One can rely on "implicitly building" that space via the behaviour of the predicates called in turn, but may become unclear
      what that space is. (This may be ok depending on coding style)
    - Using check_that/2 "normally" as a guard to make the predicate
      - throw on extremely bad arguments (i.e. badly typed ones)
      - fail on bad arguments (i.e. out-of-domain ones)
     - Logic/Search parts of the program will preferentially fail (or only enter the situation where a fail makes sense)
     - Functional programming parts of the program will preferentially throw (or only enter the situation where a throw makes sense)
  - Use check_that/2 to verify invariants inside of predicates (generally not needed as this is done by checking pre/postconditions in Prolog)
    - Expecting to switch them off at "production time" (assertions) to gain performance
    - TODO: Such cases must be marked as "this is not expected to be violated in running code" (throwing a really nasty exception)
      And then the test must be switch-offable using a specific hierarchical key, just like you switch logging on or off that way.
  - Use check_that/2 normally in code, as just a particular form of a guard, i.e. it is not expected that they will be switched off
  - (There is no easy way to perform postconditions-on-success in Prolog except by wrapping a predicate with another predicate. Annoying.)

## Assertions

  - Assertions are basically the subset of conditions that one does not expect to fail or throw at runtime.
    The idea is to remove those tests because they "never fail" and the insurance will pick up the slack if they do.
    Prolog is special in that "failing" is an integral part of its approach, so switching off checks wholesale is not an option
    (Unless one wants to really have separate instructions for "necessary checks" and "assertion checks")
    To "remove unnecessary checks" it must both be possible to:
    1) identify them. This can be done my giving them a special name, e.g. lenient_assert instead of just lenient
    2) be able to "remove them" cheaply; this can be done during compilation phase: the marked conditions can be written out
    3) select those which should be removed based on program structure: eg. all those in module XY should be removed
       this also seems a case for the compilation phase
       
## Design question especially clear in case of complex checking of lists:

  - The structure to test may have multiple layers of testing. For example, for a "proper list of char":
    - X is a var             -> fail or exception
    - X is not a proper list -> fail or exception
    - X contains vars        -> fail or exception
    - X contains non-chars   -> fails or exception
    - and finally success
    A caller could demand throwing down to any level of the above (and possibly accept var)
    The **proper way to have this flexibility** is exactly to use check_that/2 with the detailed
    more-and-more-precise check sequence, going from strictness to leniency depending on taste, for example
      check_that(X,[break(var),hard(list),hard(list(nonvar)),tuned(list(char))])
    instead of a single monolitic
      check_that(X,[break(var),hard(chars)])
    However, in that case the "rightmost checks" may perform wasteful checks against that we already
    know will succeed. So there is a need for doing bare-bones checks (maybe?). Probably not worth it.
    Note that what exception to throw is generally made in this order:
    X uninstantiated -> throw instantiation error
    X instantiated but not of the correct type (e.g. expecting integer but it's a float) -> throw type error
    X of the correct type but not in the correct domain (e.g. expecting positive integer but it's negative) -> throw domain error

## Bugs

Good:

```
?- check_that([a,_,b],hard(chars)).
ERROR: check failed : instantiation error (the culprit is not instantiated (enough))
ERROR:    message   : the value should be instantiated. Can't check for 'atom-ness'
```

```
?- check_that([a,_,b],soft(chars)).
ERROR: check failed : instantiation error (the culprit is not instantiated (enough))
ERROR:    message   : the value should be instantiated. Can't check for 'atom-ness'
```

But:

```
?- check_that([1,_,b],soft(chars)).
false.
```

because the above fails at `1`. It should throw.



