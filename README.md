# prolog_code

Hopefully organized and reusable Prolog code.

1. Subdirectory `unpacked`: Contains the source code. Source code editing proceeds here-
1. Subdirectory `prepacked`: Contains a copy of the source code arranged such that "packing" can be done
1. Subdirectory `packed`: Contains the pack files (tar files)

Note that:

- This is mostly code specific to SWI-Prolog
- README files and Prolog code use [pldoc](https://eu.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/pldoc.html%27%29) 
  markup, which is not fully compatible with github markdown. In particular, tables are not shown in github markdown, 
  so text may appear scrambled.

For information on packs, see:

- [Creating and submitting extension packages for SWI-Prolog](https://eu.swi-prolog.org/howto/Pack.html)
- [`library(prolog_pack)`](https://eu.swi-prolog.org/pldoc/man?section=prologpack)

## `pldoc` dcumentation

Available at [this URL (repo.onepointfour.be)](https://repo.onepointfour.be/pldoc/doc/_CWD_/.local/share/swi-prolog/pack/onepointfour_basics/prolog/index.html)

## How to install in SWI-Prolog

**Clone the repository** (but you get everything "in triplicate"; all the files needed are in the . tgz archive file)

```
git clone https://github.com/dtonhofer/prolog_code.git
```

You will get a file tree like this:

```
prolog_code/
├── LICENSE
├── packed
│   └── onepointfour_basics-1.0.0.tgz
├── prepacked
│   └── onepointfour_basics
│       ├── pack.pl
│       └── prolog
│           ├── meta_helpers.pl
│           ├── README.md
│           ├── safe_format.pl
│           ├── throwme_example.pl
│           └── throwme.pl
├── README.md
└── unpacked
    └── onepointfour_basics
        ├── meta_helpers.pl
        ├── README.md
        ├── safe_format.pl
        ├── throwme_example.pl
        └── throwme.pl
```

**Alternatively, you can just grab the .tgz archive file** as that is all you need.

As in:

```
$ wget https://github.com/dtonhofer/prolog_code/raw/main/packed/onepointfour_basics-1.0.1.tgz
$ file onepointfour_basics-1.0.1.tgz 
onepointfour_basics-1.0.1.tgz: gzip compressed data, last modified: Mon Feb 15 19:04:47 2021, from Unix, original size modulo 2^32 40960
```

*TODO*: Move to a separate server, along with doc. Add hashes and better GPG signature.

**Start swipl and install the package from the .tgz archive file**

```
?- pack_install('prolog_code/packed/onepointfour_basics-1.0.0.tgz').
```

Alternatively, you can cd to the directory which contains the archive file and then issue `pack_install('onepointfour_basics-1.0.0.tgz')`.

The files in the archive file have now been copied to a configuration directory in your $HOME directory. On Linux this is:

```
$HOME/.local/share/swi-prolog/pack/onepointfour_basics/
```

Take a look:

```
$ tree $HOME/.local/share/swi-prolog/pack/onepointfour_basics/

/home/paquette/.local/share/swi-prolog/pack/onepointfour_basics/
├── pack.pl
└── prolog
    ├── meta_helpers.pl
    ├── README.md
    ├── safe_format.pl
    ├── throwme_example.pl
    └── throwme.pl
```

And it's done. The diectory `$HOME/.local/share/swi-prolog/pack/onepointfour_basics/prolog` has been added to the library search path:

```
?- file_search_path(library,X),atom(X),re_match("onepointfour",X).
X = '/home/paquette/.local/share/swi-prolog/pack/onepointfour_basics/prolog' ;
false.
```

So to load the files under `prolog` (which are all module files), just do:

```
?- 
use_module(library('meta_helpers.pl')),
use_module(library('safe_format.pl')),
use_module(library('throwme.pl')).
```

This makes their predicates accessible. This also works without the quotes and
the `.pl` but I like to be clear that a file is being loaded. 
If there is a subtree under `prolog`, the paths given to `use_module/1` have to be changed accordingly.

If the files contain proper documentation text, then starting the pldoc web server in swipl:

```
?- doc_server(4000).
% Started server at http://localhost:4000/pldoc/
true.
```

and pointing the browser to 

http://localhost:4000/pldoc/doc/_CWD_/index.html

will show the pack in the drop-list at the top of the page.

(but help/1 doesn't work for the packs).

You can "remove the pack" by issuing a pack_remove/1 call:

```
?- pack_remove(onepointfour)
```

this physically removes the pack directory from disk.
