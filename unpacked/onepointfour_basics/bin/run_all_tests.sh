#!/bin/bash

# The file 
#
# ~/.config/swi-prolog/init.pl
#
# augments the library search path as it contains
#
# :- assertz(file_search_path(library,'/path/to/prolog_code/unpacked')).
#
# we can thus load module files relative to the above directory.
#
# However to consult the .plt files, we still need to be in the correct 
# directory

pushd "$HOME/Development/prolog_code/unpacked/"

for plfile in \
   checks.pl \
   dict_settings.pl \
   safe_format.pl \
   space_stringy.pl \
   stringy_and_charylist_type.pl \
   stringy_concat.pl \
   stringy_justify.pl \
   stringy_length.pl \
   stringy_morph.pl \
   stringy_overwrite.pl
do
   module_file="onepointfour_basics/$plfile"
   test_file="onepointfour_basics/${plfile}t"
   if [[ -f $test_file ]]; then
      swipl -g "use_module(library('$module_file')), list_undefined, ['$test_file'], run_tests." -t halt
   else
      echo "File $test_file does not exist; skipping"
   fi   
done

