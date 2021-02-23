# SWI-Prolog pack =onepointfour_basics=

## Contents

The pack contains the following modules:

| *Module name*                       | *File name*       | *Provides* |
| =onepointfour_basics_meta_helpers=  | =meta_helpers.pl= | Provides metapredicates to handle _switch_, _if-then-else_, _if-then_, _unless_ etc.  |
| =onepointfour_basics_safe_format=   | =safe_format.pl=  | Provides safe_format/3, a safe version of format/3.  | 
| =onepointfour_basics_throwme=       | =throwme.pl=      | Provides a predicate that helps you avoid sprinkling exception messages throughout your code.  |
| =onepointfour_basics_throwme_example= | =throwme_example.pl=  | An example for =throwme.pl=  |

## Module loading 

Copy-paste strings to load the modules. Use as goal or directive. Works as long as the parent 
directory of =onepointfour_basics= is on the library search path.

```
use_module(library('onepointfour_basics/meta_helpers.pl')).
use_module(library('onepointfour_basics/safe_format.pl')).
use_module(library('onepointfour_basics/throwme.pl')).
```

And if you want to run the example:

```
use_module(library('onepointfour_basics/throwme_example.pl')).
```

## Available tests

This pack has no tests.

## Available documentation

All modules have [pldoc](https://eu.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/pldoc.html%27%29)
documentation. 

Documentation can be exported through the SWI-Prolog documentation web server:

   1. Set the current directory of your =swipl= instance to the directory of this pack. 
      This can be done by starting =swipl= in directory =|onepointfour_basics|= or
      calling =|working_directory(Old,'/path/to/onepointfour_basics/').|=
   1. Start the documentation webserver on port 4000 (for example) by calling this goal on
      the SWI-Prolog toplevel: =|?- doc_server(4000).|=
   1. Direct your web browser to http://localhost:4000/pldoc/ 
   1. The =|README|= should appear. But the information about the Prolog files in the directory is missing.
   1. Load the modules using the use_module/1 goals given above.
   1. The information about the Prolog files appears.
   1. If there are any changes to code, calling make/0 will refresh the webserver content.

## File tree

```
onepointfour_basics/
├── meta_helpers.pl
├── README.md
├── safe_format.pl
├── throwme_example.pl
└── throwme.pl
```

## More

   @license [Zero-Clause BSD / Free Public License 1.0.0 (0BSD)](https://opensource.org/licenses/0BSD)
   @author David Tonhofer (ronerycoder@gluino.name)
   

