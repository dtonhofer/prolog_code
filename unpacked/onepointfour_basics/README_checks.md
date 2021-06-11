# `checks.pl`

A more powerful replacement for the venerable [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2).

- [`checks.pl`](checks.pl) (MIT license)
- [`checks.plt`](checks.plt) (0BSD license)

This page needs completion. For now, all the description is in [`checks.pl`](checks.pl).

## Keywords implemented so far

```
  true
  false fail
  var nonvar
  nonground ground
  atom/symbol
  atomic/constant
  compound
  boolean                            (either the atom 'true' or the atom 'false')
  pair                               (a compound term with functor name '-' and arity 2)
  string                             (an SWI-Prolog string)
  stringy                            (either an atom or an SWI-Prolog string)
  nonempty_stringy                   (nonempty stringy: a stringy that is different from '' and "")
  char                               (an atom of length 1, this is the traditional Prolog 'char' type)
  char_list,chars                    (a proper list of 0 or more chars; unbound elements are not allowed)
  code                               (any integer between 0 and 0x10FFFF meant to represent an Unicode code point)
  code_list,codes                    (a proper list of 0 or more codes; unbound elements are not allowed)
  chary                              (a char or a code)
  chary_list,charys                  (a proper list of 0 or more chars or codes (but consistently only one of those); unbound elements are not allowed)
  stringy_typeid                     (one of the atoms 'string' or 'atom'; compare to 'boolean')
  chary_typeid                       (one of the atoms 'char' or 'code'; compare to 'boolean')
  number                             (any number)
  float                              (any float, including +1.0Inf, -1.0Inf, NaN, -0.0)
  float_not_nan                      (any float, excluding NaN)
  float_not_inf                      (any float, excluding +1.0Inf, -1.0Inf)
  float_not_neginf                   (any float, excluding -1.0Inf)
  float_not_posinf                   (any float, excluding +1.0Inf)
  int/integer                        (an integer)
  rational                           (a rational, incldues integers)
  nonint_rational/proper_rational    (a rational that is not an integer)
  negnum/negnumber                   (strictly negative number)
  posnum/posnumber                   (strictly positive number)
  neg0num/neg0number                 (negative-or-zero number)
  pos0num/pos0number                 (positive-or-zero number)
  non0num/non0number                 (non-zero number)
  negint/negative_integer            (strictly negative integer)
  posint/positive_integer            (strictly positive integer)
  neg0int                            (negative-or-zero integer)
  pos0int/nonneg                     (positive-or-zero integer)
  negfloat posfloat                  (strictly negative/positive float)
  neg0float pos0float                (negative-or-zero/positive-or-zero float)
  inty                               (an integer or a float that represents an integer, e.g 1 or 1.0)
  neginty posinty                    (strictly negative/positive inty)
  neg0inty pos0inty                  (negative-or-zero/positive-or-zero inty)
  list/proper_list                   (a proper list, including the empty list)
  nonempty_list                      (a proper list that is not empty)
  dict                               (an SWI-Prolog dict)
  cyclic                             (a term that has a cyclic structure)
  acyclic                            (a term that has no cyclic structure for now, but have acquire it later unless it is also ground)
  acyclic_forever                    (a term that is both ground and acyclic)
  stream                             (a term that is a stream name (atom) or a valid stream handle (blob))
  unifies(Z)                         (unifies with Z; unification is rolled back by use \+ \+)
  member(ListOfValues)               (member of a list of values; test is unification)
  random(Probability)                (randomly fails with probability 0 =< Probability =< 1)
  forall(ListOfChecks)               (recursive: the term to check must pass all checks in ListOfChecks)
  forany(ListOfChecks)               (recursive: the term to check must pass at least one check in ListOfChecks)
```

## Examples

Fail if X is not a string (but throw if X is unbound):

```
check_that(X,[soft(string)])
```

Running it:

```
?- check_that(atom,[soft(string)]).
false.
```

Throw if X is not a string (or unbound):

```
check_that(X,[hard(string)])
```

Running it:

```
?- check_that(atom,[hard(string)]).
ERROR: check failed : type error (the culprit is not of the required type)
ERROR:    message   : the value should fulfill 'string-ness'
ERROR:    culprit   : atom
```

Throw if X is not an integer and then fail if X is not a positive integer:

```
check_that(X,[hard(int),soft(posint)])
```

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
