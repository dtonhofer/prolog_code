# Prolog Code

This repository contains Prolog Code organized using the SWI-Prolog packaging mechanism.

The code is somewhat SWI-Prolog specific.

1. Directory `unpacked`: Contains the source code. 
1. Directory `prepacked`: Contains a copy of the source code arranged such that "packing" can be done.
1. Directory `packed`: Contains the pack files (tar files)

## Currently available

These will become separate packs:

### Semver

- [README](unpacked/onepointfour_semver/README.md) - Handle "semantic version 2.0.0 strings" with DCGs.

### Basics

- [README](unpacked/onepointfour_basics/README.md) - Various basic predicates, including am SWI-Prolog 
  dict prettyprinter and a replacement for [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2) 
  called [`check_that/2`](https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/doc/README_checks.md)
