# `checks.pl`

A more powerful replacement for the venerable [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2).

- [`checks.pl`](checks.pl) (MIT license)
- [`checks.plt`](checks.plt) (0BSD license)

This page needs completion. For now, all the description is in [`checks.pl`](checks.pl).

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
