# Prolog Code

This repository contains Prolog Code organized with the SWI-Prolog packaging mechanism.

The code is somewhat SWI-Prolog specific.

1. Directory `unpacked`: Contains the source code. 
1. Directory `prepacked`: Contains a copy of the source code arranged such that "packing" can be done.
1. Directory `packed`: Contains the pack files (tar files)

## Currently available

### Semver

- [README](unpacked/onepointfour_semver/README.md) - Handle semantic version 2.0.0 strings.

### Basics

- [README](unpacked/onepointfour_basics/README.md) - Various basic predicates
   - [`checks.pl` README](unpacked/onepointfour_basics/README_checks.md) - A more powerful replacement for the venerable  [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2) to perform type and domain checks on predicate entry (among others).
   - [`space_stringy.pl` README](unpacked/onepointfour_basics/README_space_stringy.md) - Generate or accept strings or atoms consisting solely of SPACE characters.
   - [`stringy_and_charylist_type.pl` README](unpacked/onepointfour_basics/README_stringy_and_charylist_type.md) - Analyze stringys (atoms or strings) and charylists (lists of code or chars).
   - [`stringy_concat.pl` README](unpacked/onepointfour_basics/README_stringy_concat.md) - Concatenate stringys into a single stringy.
   - [`stringy_overwrite.pl` README](unpacked/onepointfour_basics/README_stringy_overwrite.md) - Overwrite a background stringy with a foreground stringy.
   - [`stringy_morph.pl` README](unpacked/onepointfour_basics/README_stringy_morph.md) - Morph atom to string and the reverse, logically.
   - [`stringy_justify.pl` README](unpacked/onepointfour_basics/README_stringy_justify.md) - UNDER CONSTRUCTION: justify a string inside a given field.

