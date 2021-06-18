# Prolog Code

This repository contains Prolog Code organized with the SWI-Prolog packaging mechanism.

The code is somewhat SWI-Prolog specific.

1. Directory `unpacked`: Contains the source code. 
1. Directory `prepacked`: Contains a copy of the source code arranged such that "packing" can be done.
1. Directory `packed`: Contains the pack files (tar files)

## Currently available

### Semver

- [unpacked/onepointfour_semver/README.md] - Handle semantic version 2.0.0 strings.

### Basics

- [unpacked/onepointfour_basics/checks.pl](README_checks.md) - A replacement for `must_be/2` to perform type and domain checks on predicate entry (among others).
- [unpacked/onepointfour_basics/space_stringy.pl](README_space_stringy.md) - Generate or accept strings or atoms consisting solely of SPACE characters.
 

